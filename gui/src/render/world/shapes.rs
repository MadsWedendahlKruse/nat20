use nat20_core::components::actions::targeting::ShapeTransform;
use parry3d::{
    na::{self, Point3},
    shape::{Shape, ShapeType, TriMesh},
};

use crate::{
    render::{
        common::utils::RenderableWithContext,
        world::mesh::{Mesh, MeshRenderMode},
    },
    state::gui_state::GuiState,
};

impl RenderableWithContext<([f32; 4], &MeshRenderMode)> for ShapeTransform {
    fn render_with_context(
        &self,
        _ui: &imgui::Ui,
        gui_state: &mut GuiState,
        (color, render_mode): ([f32; 4], &MeshRenderMode),
    ) {
        let key = get_mesh_key(&self.shape);

        let mesh = if let Some(mesh) = gui_state.mesh_cache.get(&key) {
            mesh
        } else {
            let (points, indices) = get_points_indices(&self.shape);
            let trimesh = TriMesh::new(points, indices).unwrap();
            let mesh = Mesh::from_parry_trimesh(gui_state.gl_context(), &trimesh);
            gui_state.mesh_cache.insert(key.clone(), mesh);
            gui_state.mesh_cache.get(&key).unwrap()
        };

        mesh.draw(
            gui_state.gl_context(),
            &gui_state.program,
            &self.transform.to_homogeneous(),
            color,
            render_mode,
        );
    }
}

fn get_mesh_key(shape: &Box<dyn Shape>) -> String {
    match shape.shape_type() {
        ShapeType::Ball => format!("{:?}", shape.as_ball()),
        ShapeType::Cuboid => format!("{:?}", shape.as_cuboid()),
        ShapeType::Capsule => format!("{:?}", shape.as_capsule()),
        ShapeType::Segment => format!("{:?}", shape.as_segment()),
        ShapeType::Triangle => format!("{:?}", shape.as_triangle()),
        ShapeType::Voxels => format!("{:?}", shape.as_voxels()),
        ShapeType::TriMesh => format!("{:?}", shape.as_trimesh()),
        ShapeType::Polyline => format!("{:?}", shape.as_polyline()),
        ShapeType::HalfSpace => format!("{:?}", shape.as_halfspace()),
        ShapeType::HeightField => format!("{:?}", shape.as_heightfield()),
        ShapeType::Compound => format!("{:?}", shape.as_compound()),
        ShapeType::ConvexPolyhedron => format!("{:?}", shape.as_convex_polyhedron()),
        ShapeType::Cylinder => format!("{:?}", shape.as_cylinder()),
        ShapeType::Cone => format!("{:?}", shape.as_cone()),
        ShapeType::RoundCuboid => format!("{:?}", shape.as_round_cuboid()),
        ShapeType::RoundTriangle => format!("{:?}", shape.as_round_triangle()),
        ShapeType::RoundCylinder => format!("{:?}", shape.as_round_cylinder()),
        ShapeType::RoundCone => format!("{:?}", shape.as_round_cone()),
        ShapeType::RoundConvexPolyhedron => format!("{:?}", shape.as_round_convex_polyhedron()),
        ShapeType::Custom => todo!(),
    }
}

fn get_points_indices(shape: &Box<dyn Shape>) -> (Vec<Point3<f32>>, Vec<[u32; 3]>) {
    match shape.shape_type() {
        ShapeType::Ball => {
            let ball = shape.as_ball().unwrap();
            ball.to_trimesh(8, 8)
        }
        ShapeType::Cuboid => todo!(),
        ShapeType::Capsule => todo!(),
        ShapeType::Segment => todo!(),
        ShapeType::Triangle => todo!(),
        ShapeType::Voxels => todo!(),
        ShapeType::TriMesh => todo!(),
        ShapeType::Polyline => todo!(),
        ShapeType::HalfSpace => todo!(),
        ShapeType::HeightField => todo!(),
        ShapeType::Compound => todo!(),
        ShapeType::ConvexPolyhedron => todo!(),
        ShapeType::Cylinder => todo!(),
        ShapeType::Cone => {
            let cone = shape.as_cone().unwrap();
            cone.to_trimesh(20)
        }
        ShapeType::RoundCuboid => todo!(),
        ShapeType::RoundTriangle => todo!(),
        ShapeType::RoundCylinder => todo!(),
        ShapeType::RoundCone => todo!(),
        ShapeType::RoundConvexPolyhedron => todo!(),
        ShapeType::Custom => todo!(),
    }
}

pub fn build_sphere_interleaved(
    rings: usize,
    segments: usize,
    radius: f32,
) -> (Vec<[f32; 6]>, Vec<u32>) {
    let mut verts: Vec<[f32; 6]> = Vec::new(); // [px,py,pz, nx,ny,nz]
    let mut idx: Vec<u32> = Vec::new();

    let push = |verts: &mut Vec<[f32; 6]>, p: na::Vector3<f32>, n: na::Vector3<f32>| {
        verts.push([p.x, p.y, p.z, n.x, n.y, n.z]);
    };

    for y in 0..=rings {
        let v = y as f32 / rings as f32; // 0..1
        let theta = v * std::f32::consts::PI; // polar angle from 0..pi
        let sy = theta.sin();
        let cy = theta.cos();
        for x in 0..=segments {
            let u = x as f32 / segments as f32;
            let phi = u * 2.0 * std::f32::consts::PI;
            let nx = sy * phi.cos();
            let nz = sy * phi.sin();
            let ny = cy;
            let n = na::Vector3::new(nx, ny, nz);
            let p = na::Vector3::new(nx * radius, ny * radius, nz * radius);
            push(&mut verts, p, n);
        }
    }
    // indices
    let stride = (segments + 1) as u32;
    for y in 0..rings {
        for x in 0..segments {
            let i0 = y as u32 * stride + x as u32;
            let i1 = i0 + 1;
            let i2 = i0 + stride;
            let i3 = i2 + 1;
            // triangle order CCW
            idx.extend_from_slice(&[i0, i2, i1, i1, i2, i3]);
        }
    }

    (verts, idx)
}

pub fn build_sphere_mesh(gl: &glow::Context, rings: usize, segments: usize, radius: f32) -> Mesh {
    let (verts, idx) = build_sphere_interleaved(rings, segments, radius);
    Mesh::from_interleaved(gl, &verts, &idx)
}

pub fn build_capsule_interleaved(
    rings: usize,
    segments: usize,
    radius: f32,
    half_height: f32,
) -> (Vec<[f32; 6]>, Vec<u32>) {
    // Build positions + normals for a capsule aligned along Y:
    // top hemisphere (center +half_height), cylinder, bottom hemisphere (center -half_height).
    let mut verts: Vec<[f32; 6]> = Vec::new(); // [px,py,pz, nx,ny,nz]
    let mut idx: Vec<u32> = Vec::new();

    let push = |verts: &mut Vec<[f32; 6]>, p: na::Vector3<f32>, n: na::Vector3<f32>| {
        verts.push([p.x, p.y, p.z, n.x, n.y, n.z]);
    };

    // hemispheres
    let hemi = |center_y: f32, sign: f32, verts: &mut Vec<[f32; 6]>, idx: &mut Vec<u32>| {
        let base = verts.len() as u32;
        for y in 0..=rings {
            let v = y as f32 / rings as f32; // 0..1
            // polar angle from 0..pi/2
            let theta = v * 0.5 * std::f32::consts::PI;
            let sy = theta.sin();
            let cy = theta.cos();
            for x in 0..=segments {
                let u = x as f32 / segments as f32;
                let phi = u * 2.0 * std::f32::consts::PI;
                let nx = cy * phi.cos();
                let nz = cy * phi.sin();
                let ny = sign * sy;
                let n = na::Vector3::new(nx, ny, nz);
                let p = na::Vector3::new(nx * radius, center_y + ny * radius, nz * radius);
                push(verts, p, n);
            }
        }
        // indices
        let stride = (segments + 1) as u32;
        for y in 0..rings {
            for x in 0..segments {
                let i0 = base + y as u32 * stride + x as u32;
                let i1 = i0 + 1;
                let i2 = i0 + stride;
                let i3 = i2 + 1;
                // triangle order CCW
                idx.extend_from_slice(&[i0, i2, i1, i1, i2, i3]);
            }
        }
    };

    // top hemi (center +half_height), bottom hemi (center -half_height)
    hemi(half_height, 1.0, &mut verts, &mut idx);
    // cylinder
    let base_cyl = verts.len() as u32;
    for y in 0..=1 {
        let yy = -half_height + (y as f32) * (2.0 * half_height);
        for x in 0..=segments {
            let u = x as f32 / segments as f32;
            let phi = u * 2.0 * std::f32::consts::PI;
            let n = na::Vector3::new(phi.cos(), 0.0, phi.sin());
            let p = na::Vector3::new(n.x * radius, yy, n.z * radius);
            push(&mut verts, p, n);
        }
    }
    let stride = (segments + 1) as u32;
    for x in 0..segments {
        let i0 = base_cyl + x as u32;
        let i1 = i0 + 1;
        let i2 = i0 + stride;
        let i3 = i2 + 1;
        idx.extend_from_slice(&[i0, i2, i1, i1, i2, i3]);
    }
    // bottom hemi
    hemi(-half_height, -1.0, &mut verts, &mut idx);

    (verts, idx)
}

pub fn build_capsule_mesh(
    gl: &glow::Context,
    rings: usize,
    segments: usize,
    radius: f32,
    half_height: f32,
) -> Mesh {
    let (verts, idx) = build_capsule_interleaved(rings, segments, radius, half_height);
    Mesh::from_interleaved(gl, &verts, &idx)
}
