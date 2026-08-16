//! Fills in the `Implementation` and `Test` columns of `docs/progress.md`.
//!
//! For every table row it snake_cases the feature name (`Reckless Attack` ->
//! `reckless_attack`), then finds the registry files and the `#[test]`/`#[rstest]`
//! functions whose name matches, and prints ready-to-paste cells. Rows that
//! already link everything are only summarized, and links pointing at files that
//! don't exist are reported.
//!
//! `cargo run -p nat20_core --example progress_features -- [filter] [--all]`

use std::{
    collections::BTreeSet,
    env, fs,
    path::{Path, PathBuf},
};

use nat20_core::registry::registry::REGISTRY_ROOT;

/// How the existing rows link registry files, e.g.
/// `[actions/barbarian/rage.json](../assets/registries/actions/barbarian/rage.json)`.
const LINK_PREFIX: &str = "../assets/registries/";

/// Integration tests plus the `#[cfg(test)]` modules in the engine itself.
const TEST_ROOTS: [&str; 2] = ["core/tests", "core/src"];

struct RegistryFile {
    /// Path relative to the registry root, forward slashes, e.g. `actions/barbarian/rage.json`.
    relative: String,
    stem: String,
}

struct TestFunction {
    /// Path relative to the repository root, e.g. `core/tests/class_barbarian.rs`.
    relative: String,
    name: String,
    line: usize,
}

struct Link {
    text: String,
    target: String,
}

struct Row {
    heading: String,
    level: Option<String>,
    feature: String,
    /// Every markdown link in the row, so the Test column gets checked too.
    links: Vec<Link>,
}

fn main() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
    let docs_dir = repo_root.join("docs");
    let progress_path = docs_dir.join("progress.md");

    let args: Vec<String> = env::args().skip(1).collect();
    let show_all = args.iter().any(|arg| arg == "--all");
    let filter = args
        .iter()
        .find(|arg| !arg.starts_with("--"))
        .map(|arg| arg.to_lowercase());

    let markdown = fs::read_to_string(&progress_path)
        .unwrap_or_else(|error| panic!("failed to read {:?}: {}", progress_path, error));

    let registry_files = collect_registry_files(&REGISTRY_ROOT);
    let test_functions = collect_test_functions(&repo_root);
    println!(
        "{} registry files, {} test functions\n",
        registry_files.len(),
        test_functions.len()
    );

    let mut up_to_date = 0;
    let mut incomplete = 0;
    let mut unimplemented = 0;
    let mut untested = 0;
    let mut broken_links = 0;
    let mut stale_anchors = 0;

    for row in parse_rows(&markdown) {
        if let Some(filter) = &filter {
            let haystack = format!("{} {}", row.heading, row.feature).to_lowercase();
            if !haystack.contains(filter) {
                continue;
            }
        }

        let snake = snake_case(&row.feature);
        let matched_files: Vec<&RegistryFile> = registry_files
            .iter()
            .filter(|file| stem_matches(&file.stem, &snake))
            .collect();
        let matched_tests: Vec<&TestFunction> = test_functions
            .iter()
            .filter(|test| stem_matches(&test.name, &snake))
            .collect();

        // Link targets carry a `#L42` anchor once tests are linked per function,
        // but the row is only ever compared at file granularity.
        let linked: BTreeSet<&str> = row
            .links
            .iter()
            .map(|link| link_path(&link.target))
            .collect();

        let missing_files: Vec<&&RegistryFile> = matched_files
            .iter()
            .filter(|file| !linked.contains(registry_link(file).as_str()))
            .collect();

        let test_files: Vec<&str> = unique_test_files(&matched_tests);
        let missing_test_files: Vec<&&str> = test_files
            .iter()
            .filter(|relative| !linked.contains(repo_link(relative).as_str()))
            .collect();

        let expected: BTreeSet<String> = matched_files
            .iter()
            .map(|file| registry_link(file))
            .chain(test_files.iter().map(|relative| repo_link(relative)))
            .collect();
        let dangling: Vec<&Link> = row
            .links
            .iter()
            .filter(|link| !docs_dir.join(link_path(&link.target)).exists())
            .collect();
        broken_links += dangling.len();

        let stale: Vec<String> = row
            .links
            .iter()
            .filter_map(|link| stale_anchor(link, &test_functions))
            .collect();
        stale_anchors += stale.len();

        // A link that doesn't resolve is already reported as broken, no need to
        // also point out that it doesn't match the feature name.
        let unmatched: Vec<&&str> = linked
            .iter()
            .filter(|path| {
                !expected.contains(**path)
                    && !dangling
                        .iter()
                        .any(|link| link_path(&link.target) == **path)
            })
            .collect();

        let label = match &row.level {
            Some(level) => format!("{} / {} (level {})", row.heading, row.feature, level),
            None => format!("{} / {}", row.heading, row.feature),
        };
        let mut printed = false;
        let mut header = |printed: &mut bool| {
            if !*printed {
                println!("{}", label);
                *printed = true;
            }
        };

        if matched_files.is_empty() {
            unimplemented += 1;
            if show_all {
                header(&mut printed);
                println!("  implementation: no registry files match `{}`", snake);
            }
        } else if missing_files.is_empty() {
            up_to_date += 1;
            if show_all {
                header(&mut printed);
                println!("  implementation: up to date ({} files)", matched_files.len());
            }
        } else {
            incomplete += 1;
            header(&mut printed);
            println!("  implementation: {} file(s) not linked:", missing_files.len());
            for file in &missing_files {
                println!("    + {}", file.relative);
            }
            println!("  paste into Implementation:");
            println!("    {}", implementation_cell(&matched_files));
        }

        if matched_tests.is_empty() {
            // Only worth flagging once something is actually implemented.
            if !matched_files.is_empty() {
                untested += 1;
                header(&mut printed);
                println!("  tests: none match `{}`", snake);
            }
        } else if missing_test_files.is_empty() && stale.is_empty() {
            if show_all {
                header(&mut printed);
                println!(
                    "  tests: up to date ({} in {} file(s))",
                    matched_tests.len(),
                    test_files.len()
                );
            }
        } else {
            header(&mut printed);
            println!(
                "  tests: {} matching, {} file(s) not linked, {} stale anchor(s):",
                matched_tests.len(),
                missing_test_files.len(),
                stale.len()
            );
            for test in &matched_tests {
                println!("    + {}::{}", test.relative, test.name);
            }
            for issue in &stale {
                println!("    ! {}", issue);
            }
            println!("  paste into Test:");
            println!("    {}", test_cell(&matched_tests));
        }

        for link in &dangling {
            header(&mut printed);
            println!("  ! link target does not exist: {}", link.target);
        }
        for path in &unmatched {
            header(&mut printed);
            println!("  ? linked but not matched by `{}`: {}", snake, path);
        }

        if printed {
            println!();
        }
    }

    println!(
        "{} up to date, {} incomplete, {} without registry files, {} untested, {} broken link(s), {} stale anchor(s)",
        up_to_date, incomplete, unimplemented, untested, broken_links, stale_anchors
    );
    if !show_all {
        println!("(pass --all to also list the rows that are already complete)");
    }
}

fn collect_registry_files(root: &Path) -> Vec<RegistryFile> {
    let mut files = Vec::new();
    collect_registry_files_recursive(root, root, &mut files);
    files.sort_by(|a, b| a.relative.cmp(&b.relative));
    files
}

fn collect_registry_files_recursive(root: &Path, directory: &Path, files: &mut Vec<RegistryFile>) {
    let entries = fs::read_dir(directory)
        .unwrap_or_else(|error| panic!("failed to read {:?}: {}", directory, error));

    for entry in entries {
        let path = entry
            .unwrap_or_else(|error| panic!("failed to read entry in {:?}: {}", directory, error))
            .path();

        let name = path
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("");
        // Skips `.schemas` and `.luastubs`, which aren't content.
        if name.starts_with('.') {
            continue;
        }

        if path.is_dir() {
            collect_registry_files_recursive(root, &path, files);
            continue;
        }

        let extension = path.extension().and_then(|ext| ext.to_str());
        if !matches!(extension, Some("json") | Some("lua")) {
            continue;
        }

        let relative = path
            .strip_prefix(root)
            .expect("registry file outside the registry root")
            .to_string_lossy()
            .replace('\\', "/");
        let stem = path
            .file_stem()
            .and_then(|stem| stem.to_str())
            .unwrap_or_default()
            .to_string();

        files.push(RegistryFile { relative, stem });
    }
}

fn collect_test_functions(repo_root: &Path) -> Vec<TestFunction> {
    let mut tests = Vec::new();

    for root in TEST_ROOTS {
        let directory = repo_root.join(root);
        let mut sources = Vec::new();
        collect_rust_sources(&directory, &mut sources);

        for source in sources {
            let relative = format!(
                "{}/{}",
                root,
                source
                    .strip_prefix(&directory)
                    .expect("source outside the test root")
                    .to_string_lossy()
                    .replace('\\', "/")
            );
            let contents = fs::read_to_string(&source)
                .unwrap_or_else(|error| panic!("failed to read {:?}: {}", source, error));
            collect_test_functions_in_source(&relative, &contents, &mut tests);
        }
    }

    tests
}

fn collect_rust_sources(directory: &Path, sources: &mut Vec<PathBuf>) {
    let entries = fs::read_dir(directory)
        .unwrap_or_else(|error| panic!("failed to read {:?}: {}", directory, error));

    for entry in entries {
        let path = entry
            .unwrap_or_else(|error| panic!("failed to read entry in {:?}: {}", directory, error))
            .path();

        if path.is_dir() {
            collect_rust_sources(&path, sources);
        } else if path.extension().and_then(|ext| ext.to_str()) == Some("rs") {
            sources.push(path);
        }
    }
}

/// A `#[test]`/`#[rstest]` attribute followed by the next `fn`, so the `#[case(..)]`
/// lines of an rstest in between are ignored and the whole thing counts once.
fn collect_test_functions_in_source(relative: &str, contents: &str, tests: &mut Vec<TestFunction>) {
    let mut is_test = false;

    for (index, line) in contents.lines().enumerate() {
        let trimmed = line.trim_start();

        if trimmed.starts_with("#[test]") || trimmed.starts_with("#[rstest") {
            is_test = true;
            continue;
        }

        if !trimmed.starts_with('#') {
            if is_test && let Some(name) = function_name(trimmed) {
                tests.push(TestFunction {
                    relative: relative.to_string(),
                    name,
                    line: index + 1,
                });
            }
            is_test = false;
        }
    }
}

fn function_name(line: &str) -> Option<String> {
    let rest = line
        .strip_prefix("pub ")
        .unwrap_or(line)
        .strip_prefix("async ")
        .unwrap_or(line)
        .strip_prefix("fn ")?;
    let name: String = rest
        .chars()
        .take_while(|c| c.is_alphanumeric() || *c == '_')
        .collect();
    (!name.is_empty()).then_some(name)
}

/// Checks a `file.rs#L42` link against where the test actually sits now. The link
/// text carries the name (`class_barbarian.rs::rage_blocks_spellcasting`), so a
/// test that moved can be told apart from one that was renamed or deleted.
fn stale_anchor(link: &Link, tests: &[TestFunction]) -> Option<String> {
    let anchor = link.target.split_once("#L")?.1;
    let line: usize = anchor.parse().ok()?;

    let relative = link_path(&link.target).strip_prefix("../")?;
    if !relative.ends_with(".rs") {
        return None;
    }

    let at_line = tests
        .iter()
        .find(|test| test.relative == relative && test.line == line);

    let Some((_, name)) = link.text.split_once("::") else {
        return at_line
            .is_none()
            .then(|| format!("{} has no test at L{}", relative, line));
    };

    if at_line.is_some_and(|test| test.name == name) {
        return None;
    }

    match tests
        .iter()
        .find(|test| test.relative == relative && test.name == name)
    {
        Some(test) => Some(format!(
            "{}::{} is at L{}, linked as L{}",
            relative, name, test.line, line
        )),
        None => Some(format!("{} has no test named {}", relative, name)),
    }
}

fn unique_test_files<'a>(tests: &[&'a TestFunction]) -> Vec<&'a str> {
    let mut files: Vec<&str> = tests.iter().map(|test| test.relative.as_str()).collect();
    files.dedup();
    files
}

/// The path half of a link target, without the `#L42` anchor.
fn link_path(link: &str) -> &str {
    link.split('#').next().unwrap_or(link)
}

fn registry_link(file: &RegistryFile) -> String {
    format!("{}{}", LINK_PREFIX, file.relative)
}

fn repo_link(relative: &str) -> String {
    format!("../{}", relative)
}

fn parse_rows(markdown: &str) -> Vec<Row> {
    let mut rows = Vec::new();
    let mut heading = String::new();
    let mut columns: Option<Vec<String>> = None;

    for line in markdown.lines() {
        let trimmed = line.trim();

        if let Some(text) = trimmed.strip_prefix('#') {
            heading = text.trim_start_matches('#').trim().to_string();
            columns = None;
            continue;
        }

        if !trimmed.starts_with('|') {
            columns = None;
            continue;
        }

        let cells: Vec<String> = split_cells(trimmed);

        // A separator row (`| --- | --- |`) sits between the header and the data.
        if cells
            .iter()
            .all(|cell| !cell.is_empty() && cell.chars().all(|c| c == '-' || c == ':'))
        {
            continue;
        }

        let Some(header) = &columns else {
            if cells.iter().any(|cell| cell == "Feature") {
                columns = Some(cells);
            }
            continue;
        };

        let cell = |name: &str| {
            header
                .iter()
                .position(|column| column == name)
                .and_then(|index| cells.get(index))
                .cloned()
                .unwrap_or_default()
        };

        let feature = cell("Feature");
        if feature.is_empty() {
            continue;
        }

        let level = Some(cell("Level")).filter(|level| !level.is_empty());
        let links = cells.iter().flat_map(|cell| markdown_links(cell)).collect();

        rows.push(Row {
            heading: heading.clone(),
            level,
            feature,
            links,
        });
    }

    rows
}

fn split_cells(line: &str) -> Vec<String> {
    line.trim_start_matches('|')
        .trim_end_matches('|')
        .split('|')
        .map(|cell| cell.trim().to_string())
        .collect()
}

fn markdown_links(cell: &str) -> Vec<Link> {
    let mut links = Vec::new();
    let mut rest = cell;

    while let Some(separator) = rest.find("](") {
        let (before, after) = (&rest[..separator], &rest[separator + 2..]);
        let Some(end) = after.find(')') else {
            break;
        };

        let text = before
            .rfind('[')
            .map(|start| before[start + 1..].to_string())
            .unwrap_or_default();
        links.push(Link {
            text,
            target: after[..end].to_string(),
        });
        rest = &after[end + 1..];
    }

    links
}

fn snake_case(feature: &str) -> String {
    let mut snake = String::new();
    for character in feature.chars() {
        if character.is_alphanumeric() {
            snake.extend(character.to_lowercase());
        } else if !snake.ends_with('_') {
            snake.push('_');
        }
    }
    snake.trim_matches('_').to_string()
}

/// Matches on whole `_`-separated words, so `rage` picks up `extend_rage` and
/// `brutal_strike` picks up `improved_brutal_strike`, without matching substrings
/// in the middle of a word.
fn stem_matches(stem: &str, feature: &str) -> bool {
    stem == feature
        || stem.starts_with(&format!("{}_", feature))
        || stem.ends_with(&format!("_{}", feature))
        || stem.contains(&format!("_{}_", feature))
}

fn implementation_cell(files: &[&RegistryFile]) -> String {
    files
        .iter()
        .map(|file| format!("[{}]({})", file.relative, registry_link(file)))
        .collect::<Vec<_>>()
        .join("<br>")
}

fn test_cell(tests: &[&TestFunction]) -> String {
    tests
        .iter()
        .map(|test| {
            let file_name = test.relative.rsplit('/').next().unwrap_or(&test.relative);
            format!(
                "[{}::{}]({}#L{})",
                file_name,
                test.name,
                repo_link(&test.relative),
                test.line
            )
        })
        .collect::<Vec<_>>()
        .join("<br>")
}
