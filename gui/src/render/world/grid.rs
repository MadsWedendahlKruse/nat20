// render/grid_renderer.rs
use glow::HasContext;
use parry3d::na;

pub struct GridRenderer {
    program: glow::Program,
    vao: glow::VertexArray,
    vbo: glow::Buffer,
    count: i32,
}

impl GridRenderer {
    /// extent: how far in +/− units (e.g., 10 -> grid from −10..+10)
    /// step: spacing between lines (e.g., 1.0)
    /// major_every: draw slightly brighter lines every N steps (e.g., 10)
    pub fn new(
        gl: &glow::Context,
        extent: i32,
        step: f32,
        major_every: i32,
        vert_src: &str,
        frag_src: &str,
    ) -> Self {
        // --- compile program ---
        unsafe fn compile(gl: &glow::Context, ty: u32, src: &str) -> glow::Shader {
            unsafe {
                let sh = gl.create_shader(ty).unwrap();
                gl.shader_source(sh, src);
                gl.compile_shader(sh);
                assert!(
                    gl.get_shader_compile_status(sh),
                    "shader: {}",
                    gl.get_shader_info_log(sh)
                );
                sh
            }
        }
        let program = unsafe {
            let vs = compile(gl, glow::VERTEX_SHADER, vert_src);
            let fs = compile(gl, glow::FRAGMENT_SHADER, frag_src);
            let prog = gl.create_program().unwrap();
            gl.attach_shader(prog, vs);
            gl.attach_shader(prog, fs);
            gl.link_program(prog);
            assert!(
                gl.get_program_link_status(prog),
                "link: {}",
                gl.get_program_info_log(prog)
            );
            gl.delete_shader(vs);
            gl.delete_shader(fs);
            prog
        };

        // --- build line list on XZ plane ---
        let e = extent.max(1);
        let eps_y = 0.005f32; // lift a hair to avoid z-fighting
        let mut verts: Vec<f32> = Vec::new(); // [px,py,pz, r,g,b] per vertex

        let mut push_line = |a: na::Vector3<f32>, b: na::Vector3<f32>, col: [f32; 3]| {
            verts.extend_from_slice(&[a.x, a.y, a.z, col[0], col[1], col[2]]);
            verts.extend_from_slice(&[b.x, b.y, b.z, col[0], col[1], col[2]]);
        };

        for i in -e..=e {
            let x = i as f32 * step;
            let z = i as f32 * step;

            // colors
            let is_major = i % major_every == 0;
            let minor = [0.25, 0.25, 0.3];
            let major = [0.45, 0.45, 0.55];
            let col = if is_major { major } else { minor };

            // lines parallel to Z (varying X)
            push_line(
                na::Vector3::new(x, eps_y, -e as f32 * step),
                na::Vector3::new(x, eps_y, e as f32 * step),
                col,
            );
            // lines parallel to X (varying Z)
            push_line(
                na::Vector3::new(-e as f32 * step, eps_y, z),
                na::Vector3::new(e as f32 * step, eps_y, z),
                col,
            );
        }

        // axis lines (X red, Z blue, Y green up-stem at origin)
        push_line(
            na::Vector3::new(0.0, 2.0 * eps_y, 0.0),
            na::Vector3::new(1.0, 2.0 * eps_y, 0.0),
            [0.85, 0.2, 0.2],
        );
        push_line(
            na::Vector3::new(0.0, 2.0 * eps_y, 0.0),
            na::Vector3::new(0.0, 2.0 * eps_y, 1.0),
            [0.2, 0.4, 0.9],
        );
        // a small Y axis stem at origin so you can see "up"
        push_line(
            na::Vector3::new(0.0, 0.0, 0.0),
            na::Vector3::new(0.0, 1.0, 0.0),
            [0.2, 0.8, 0.2],
        );

        // --- upload (no indices; GL_LINES) ---
        let (vao, vbo);
        unsafe {
            vao = gl.create_vertex_array().unwrap();
            vbo = gl.create_buffer().unwrap();
            gl.bind_vertex_array(Some(vao));

            gl.bind_buffer(glow::ARRAY_BUFFER, Some(vbo));
            gl.buffer_data_u8_slice(
                glow::ARRAY_BUFFER,
                bytemuck::cast_slice(&verts),
                glow::STATIC_DRAW,
            );

            // pos at loc 0
            gl.enable_vertex_attrib_array(0);
            gl.vertex_attrib_pointer_f32(0, 3, glow::FLOAT, false, 6 * 4, 0);
            // color at loc 1
            gl.enable_vertex_attrib_array(1);
            gl.vertex_attrib_pointer_f32(1, 3, glow::FLOAT, false, 6 * 4, 3 * 4);

            gl.bind_vertex_array(None);
            gl.bind_buffer(glow::ARRAY_BUFFER, None);
        }

        Self {
            program,
            vao,
            vbo,
            count: (verts.len() / 6) as i32,
        }
    }

    pub fn draw(&self, gl: &glow::Context) {
        unsafe {
            gl.use_program(Some(self.program));
            gl.bind_vertex_array(Some(self.vao));
            gl.line_width(1.0);
            gl.draw_arrays(glow::LINES, 0, self.count);
            gl.bind_vertex_array(None);
            gl.use_program(None);
        }
    }

    pub fn destroy(&self, gl: &glow::Context) {
        unsafe {
            gl.delete_vertex_array(self.vao);
            gl.delete_buffer(self.vbo);
            gl.delete_program(self.program);
        }
    }
}
