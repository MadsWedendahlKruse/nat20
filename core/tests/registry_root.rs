extern crate nat20_core;

mod tests {
    use std::{fs, path::Path, path::PathBuf};

    use nat20_core::{
        components::id::ItemId,
        registry::registry::{ItemsRegistry, REGISTRY_ROOT, REGISTRY_ROOT_ENV_VAR, registry_root},
    };

    fn copy_dir(from: &Path, to: &Path) {
        fs::create_dir_all(to).unwrap();
        for entry in fs::read_dir(from).unwrap() {
            let entry = entry.unwrap();
            let target = to.join(entry.file_name());
            if entry.file_type().unwrap().is_dir() {
                copy_dir(&entry.path(), &target);
            } else {
                fs::copy(entry.path(), &target).unwrap();
            }
        }
    }

    #[test]
    fn registry_root_defaults_to_assets() {
        let default_root = registry_root(None);
        assert!(default_root.ends_with("assets/registries"));
        assert_eq!(registry_root(Some("".into())), default_root);
    }

    #[test]
    fn registry_root_uses_override() {
        assert_eq!(
            registry_root(Some("/some/other/registries".into())),
            PathBuf::from("/some/other/registries")
        );
    }

    // This is the only test in this file that touches the registries, since
    // they're loaded once per process and the environment variable has to be
    // set before that happens.
    #[test]
    fn registries_load_from_env_var_root() {
        let root = std::env::temp_dir()
            .join(format!("nat20_registry_root_{}", std::process::id()))
            .join("registries");
        let _ = fs::remove_dir_all(&root);
        copy_dir(&registry_root(None), &root);

        // Add an item that only exists in the copy, so finding it proves the
        // registries were loaded from there.
        let dagger = fs::read_to_string(root.join("items/dagger.json")).unwrap();
        fs::write(
            root.join("items/copied_dagger.json"),
            dagger.replace("item.dagger", "item.copied_dagger"),
        )
        .unwrap();

        // SAFETY: no other test in this process reads the environment.
        unsafe { std::env::set_var(REGISTRY_ROOT_ENV_VAR, &root) };

        assert_eq!(*REGISTRY_ROOT, root);
        assert!(ItemsRegistry::get(&ItemId::new("nat20_core", "item.copied_dagger")).is_some());
        assert!(ItemsRegistry::get(&ItemId::new("nat20_core", "item.dagger")).is_some());

        fs::remove_dir_all(root.parent().unwrap()).unwrap();
    }
}
