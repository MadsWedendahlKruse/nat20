use std::{fs::File, io::BufReader, path::Path};

use glam::{UVec3, Vec2, Vec3A};
use obj::Obj;
use parry3d::na::Point3;
use polyanya::Coords;
use rerecast::{
    AreaType, BuildContoursFlags, Config, DetailNavmesh, HeightfieldBuilder, PolygonNavmesh,
};
use serde::{Deserialize, Serialize};
use tracing::debug;
use uom::si::{f32::Length, length::meter};

use crate::systems::geometry::EPSILON;

#[derive(Serialize, Deserialize)]
pub struct WorldGeometry {
    points: Vec<[f32; 3]>,
    indices: Vec<[u32; 3]>,
    pub trimesh: parry3d::shape::TriMesh,
    pub poly_navmesh: PolygonNavmesh,
    pub detail_navmesh: DetailNavmesh,
    pub polyanya_mesh: polyanya::Mesh,
}

impl WorldGeometry {
    pub fn new(points: Vec<[f32; 3]>, indices: Vec<[u32; 3]>, config: &Config) -> Self {
        let (poly_navmesh, detail_navmesh, polyanya_mesh) =
            build_navmesh(&points, &indices, config);

        Self {
            points: points.clone(),
            indices: indices.clone(),
            trimesh: parry3d::shape::TriMesh::new(
                points.iter().map(|p| Point3::from_slice(p)).collect(),
                indices.clone(),
            )
            .unwrap(),
            poly_navmesh,
            detail_navmesh,
            polyanya_mesh,
        }
    }

    pub fn from_obj(obj: Obj, config: &Config) -> Self {
        let points = obj
            .vertices
            .iter()
            .map(|v| [v.position[0], v.position[1], v.position[2]])
            .collect();

        let indices = obj
            .indices
            .chunks(3)
            .map(|chunk| [chunk[0] as u32, chunk[1] as u32, chunk[2] as u32])
            .collect();

        Self::new(points, indices, config)
    }

    pub fn from_obj_path(obj_path: impl AsRef<Path>, config: &Config) -> Self {
        let obj: Obj = obj::load_obj(BufReader::new(File::open(obj_path).unwrap()))
            .expect("Failed to load world geometry");
        Self::from_obj(obj, config)
    }

    pub fn rebuild_navmesh(&mut self, config: &Config) {
        let (poly_navmesh, detail_navmesh, polyanya_mesh) =
            build_navmesh(&self.points, &self.indices, config);

        self.poly_navmesh = poly_navmesh;
        self.detail_navmesh = detail_navmesh;
        self.polyanya_mesh = polyanya_mesh;
    }

    pub(crate) fn path(&self, start: Point3<f32>, end: Point3<f32>) -> Option<WorldPath> {
        // Path found with pathfinding doesn't include start, so add it manually
        let mut final_path = vec![start];

        let start_coords = self
            .polyanya_mesh
            .get_closest_point_at_height(Coords::on_mesh(Vec2::new(start.x, start.z)), start.y)?;
        let end_coords = self
            .polyanya_mesh
            .get_closest_point_at_height(Coords::on_mesh(Vec2::new(end.x, end.z)), end.y)?;

        self.polyanya_mesh
            .path(start_coords, end_coords)
            .map(|path| {
                final_path.extend(
                    path.path_with_height(
                        [start.x, start.y, start.z].into(),
                        [end.x, end.y, end.z].into(),
                        &self.polyanya_mesh,
                    )
                    .into_iter()
                    .map(|p| Point3::new(p.x, p.y, p.z)),
                );

                WorldPath::new(final_path)
            })
    }
}

fn build_navmesh(
    points: &Vec<[f32; 3]>,
    indices: &Vec<[u32; 3]>,
    config: &Config,
) -> (PolygonNavmesh, DetailNavmesh, polyanya::Mesh) {
    // See: https://github.com/janhohenheim/rerecast/blob/main/crates/rerecast/tests/cpp_comparison.rs
    // For params see: https://github.com/janhohenheim/rerecast/blob/main/crates/rerecast/src/config.rs
    // see also: https://www.youtube.com/watch?v=wYRrvWaLjJ8

    let start_time = std::time::Instant::now();
    let mut nav_trimesh = rerecast::TriMesh {
        vertices: points
            .iter()
            .map(|p| Vec3A::new(p[0], p[1], p[2]))
            .collect(),
        indices: indices
            .iter()
            .map(|i| UVec3::new(i[0], i[1], i[2]))
            .collect(),
        area_types: vec![AreaType::DEFAULT_WALKABLE; indices.len()],
    };
    debug!("Created nav trimesh in {:?}", start_time.elapsed());

    let start_time = std::time::Instant::now();
    nav_trimesh.mark_walkable_triangles(f32::to_radians(45.0));
    debug!("Marked walkable triangles in {:?}", start_time.elapsed());

    let start_time = std::time::Instant::now();
    let aabb = nav_trimesh.compute_aabb().unwrap();
    debug!("Computed AABB in {:?}", start_time.elapsed());

    let start_time = std::time::Instant::now();
    let mut heightfield = HeightfieldBuilder {
        aabb,
        cell_size: config.cell_size,
        cell_height: config.cell_height,
    }
    .build()
    .unwrap();
    debug!("Built heightfield in {:?}", start_time.elapsed());

    let start_time = std::time::Instant::now();
    heightfield
        .rasterize_triangles(&nav_trimesh, config.walkable_climb)
        .unwrap();
    debug!("Rasterized triangles in {:?}", start_time.elapsed());

    let start_time = std::time::Instant::now();
    // Once all geometry is rasterized, we do initial pass of filtering to
    // remove unwanted overhangs caused by the conservative rasterization
    // as well as filter spans where the character cannot possibly stand.
    heightfield.filter_low_hanging_walkable_obstacles(config.walkable_climb);
    heightfield.filter_ledge_spans(config.walkable_height, config.walkable_climb);
    heightfield.filter_walkable_low_height_spans(config.walkable_height);
    debug!("Filtered walkable surfaces in {:?}", start_time.elapsed());

    let start_time = std::time::Instant::now();
    let mut compact_heightfield = heightfield
        .into_compact(config.walkable_height, config.walkable_climb)
        .unwrap();
    debug!(
        "Converted to compact heightfield in {:?}",
        start_time.elapsed()
    );

    let start_time = std::time::Instant::now();
    compact_heightfield.erode_walkable_area(config.walkable_radius);
    debug!("Eroded walkable area in {:?}", start_time.elapsed());

    let start_time = std::time::Instant::now();
    compact_heightfield.build_distance_field();
    debug!("Built distance field in {:?}", start_time.elapsed());

    let start_time = std::time::Instant::now();
    compact_heightfield
        .build_regions(
            config.border_size,
            config.min_region_area,
            config.merge_region_area,
        )
        .unwrap();
    debug!("Built compact heightfield in {:?}", start_time.elapsed());

    let start_time = std::time::Instant::now();
    let contours = compact_heightfield.build_contours(
        config.max_simplification_error,
        config.max_edge_len,
        BuildContoursFlags::DEFAULT,
    );
    debug!("Built contours in {:?}", start_time.elapsed());

    debug!("[WorldGeometry] Creating navmesh",);
    let build_start_time = std::time::Instant::now();
    let poly_navmesh = contours
        .into_polygon_mesh(config.max_vertices_per_polygon)
        .unwrap();

    let detail_navmesh = DetailNavmesh::new(
        &poly_navmesh,
        &compact_heightfield,
        config.detail_sample_dist,
        config.detail_sample_max_error,
    )
    .unwrap();

    debug!("Navmesh built in {:?}", build_start_time.elapsed());

    let mut polyanya_mesh: polyanya::Mesh =
        polyanya::RecastFullMesh::new(poly_navmesh.clone(), detail_navmesh.clone()).into();
    // Increase search steps to allow snapping points to navmesh from further away
    // polyanya_mesh.search_steps *= 10;

    (poly_navmesh, detail_navmesh, polyanya_mesh)
}

#[derive(Debug, Clone, PartialEq)]
pub struct WorldPath {
    pub points: Vec<Point3<f32>>,
    pub length: Length,
}

impl WorldPath {
    pub fn new(points: Vec<Point3<f32>>) -> Self {
        let length = if points.len() < 2 {
            Length::new::<meter>(0.0)
        } else {
            let mut total_length = 0.0;
            for i in 0..(points.len() - 1) {
                total_length += (points[i + 1] - points[i]).magnitude();
            }

            Length::new::<meter>(total_length)
        };

        Self { points, length }
    }

    pub fn trim_to_length(&self, max_length: Length) -> Self {
        if self.length <= max_length {
            return self.clone();
        }

        let mut trimmed_points = Vec::new();
        let mut accumulated_length = Length::new::<meter>(0.0);

        for i in 0..(self.points.len() - 1) {
            let segment_start = self.points[i];
            let segment_end = self.points[i + 1];
            let segment_vector = segment_end - segment_start;
            let segment_length = Length::new::<meter>(segment_vector.magnitude());

            trimmed_points.push(segment_start);
            if accumulated_length + segment_length <= max_length {
                accumulated_length += segment_length;
            } else {
                let remaining_length = max_length - accumulated_length;
                let t = remaining_length.get::<meter>() / segment_length.get::<meter>();
                let new_point = segment_start + segment_vector * t;
                trimmed_points.push(new_point);
                accumulated_length = max_length;
                break;
            }
        }

        // If we didn't reach max_length, include the last point
        if accumulated_length < max_length && !self.points.is_empty() {
            trimmed_points.push(*self.points.last().unwrap());
        }

        Self::new(trimmed_points)
    }

    pub fn start(&self) -> Option<&Point3<f32>> {
        self.points.first()
    }

    pub fn end(&self) -> Option<&Point3<f32>> {
        self.points.last()
    }

    pub fn distance_along_path(&self, point: &Point3<f32>) -> Option<Length> {
        let mut accumulated_length = 0.0;

        for i in 0..(self.points.len() - 1) {
            let segment_start = self.points[i];
            let segment_end = self.points[i + 1];
            let segment_vector = segment_end - segment_start;
            let segment_length = segment_vector.magnitude();

            if segment_length == 0.0 {
                if (point - segment_start).magnitude() <= EPSILON {
                    return Some(Length::new::<meter>(accumulated_length));
                }
                continue;
            }

            let t = (point - segment_start).dot(&segment_vector) / segment_length.powi(2);
            if t >= 0.0 && t <= 1.0 {
                let projection = segment_start + segment_vector * t;
                if (point - projection).magnitude() <= EPSILON {
                    return Some(Length::new::<meter>(
                        accumulated_length + segment_length * t,
                    ));
                }
            }

            accumulated_length += segment_length;
        }

        None
    }

    pub fn point_at_distance(&self, distance: Length) -> Option<Point3<f32>> {
        let distance = distance.get::<meter>();
        let mut accumulated_length = 0.0;

        for i in 0..(self.points.len() - 1) {
            let segment_start = self.points[i];
            let segment_end = self.points[i + 1];
            let segment_vector = segment_end - segment_start;
            let segment_length = segment_vector.magnitude();

            if accumulated_length + segment_length >= distance {
                let remaining_length = distance - accumulated_length;
                let t = remaining_length / segment_length;
                return Some(segment_start + segment_vector * t);
            }

            accumulated_length += segment_length;
        }

        // If distance exceeds path length, return the last point
        self.points.last().cloned()
    }

    /// Splits the path into multiple paths at the specified split points.
    /// The split points must lie on the path and must be sorted in the order
    /// they appear on the path.
    pub fn split_at_points(&self, split_points: Vec<Point3<f32>>) -> Vec<WorldPath> {
        if split_points.is_empty() {
            return vec![self.clone()];
        }

        let mut paths = Vec::new();
        let mut current_points = Vec::new();
        let mut split_iter = split_points.iter().peekable();

        for i in 0..(self.points.len() - 1) {
            let segment_start = self.points[i];
            let segment_end = self.points[i + 1];
            current_points.push(segment_start);

            while let Some(split_point) = split_iter.peek() {
                if Self::is_point_on_segment(split_point, &segment_start, &segment_end) {
                    current_points.push(**split_point);
                    paths.push(WorldPath::new(current_points.clone()));
                    current_points.clear();
                    split_iter.next();
                } else {
                    break;
                }
            }
        }

        if let Some(last_point) = self.points.last() {
            current_points.push(*last_point);
        }

        if !current_points.is_empty() {
            paths.push(WorldPath::new(current_points));
        }

        paths
    }

    fn is_point_on_segment(point: &Point3<f32>, start: &Point3<f32>, end: &Point3<f32>) -> bool {
        let segment_vector = *end - *start;
        let point_vector = *point - *start;

        let segment_length = segment_vector.magnitude();
        if segment_length == 0.0 {
            return (point - start).magnitude() <= EPSILON;
        }

        let t = point_vector.dot(&segment_vector) / segment_length.powi(2);
        if t < 0.0 || t > 1.0 {
            return false;
        }

        let projection = *start + segment_vector * t;
        (point - projection).magnitude() <= EPSILON
    }
}
