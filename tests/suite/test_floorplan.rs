//! Floorplan regions written in the source (`constraint physical {
//! floorplan { region ... } }`): areas in percent of the die, names
//! resolved to the instances and signals of the elaborated design.

use skalp_frontend::hir::GlobalConstraint;
use skalp_frontend::parse_and_build_hir;
use skalp_lir::floorplan::{resolve_floorplan, FloorplanMember};

fn source(region: &str) -> String {
    format!(
        r#"
        constraint physical {{
            floorplan {{
                {region}
            }}
        }}

        entity Inner {{
            in clk: clock
            out q: bit
        }}

        impl Inner {{
            signal t: bit = 0
            on(clk.rise) {{ t = ~t }}
            q = t
        }}

        entity Top {{
            in clk: clock
            out a: bit
            out b: bit
        }}

        impl Top {{
            signal ring: bit = 0
            on(clk.rise) {{ ring = ~ring }}
            a = ring
            inst core = Inner {{ clk: clk }}
            b = core.q
        }}
        "#
    )
}

fn resolve(region: &str) -> Result<Vec<skalp_lir::floorplan::FloorplanRegion>, String> {
    let hir = parse_and_build_hir(&source(region)).map_err(|e| e.to_string())?;
    let mir = skalp_mir::MirCompiler::new()
        .with_optimization_level(skalp_mir::OptimizationLevel::None)
        .compile_to_mir(&hir)?;
    let hierarchy = skalp_lir::lower_mir_hierarchical(&mir);
    resolve_floorplan(&hir, &mir, &hierarchy)
}

#[test]
fn a_region_holds_the_signals_and_instances_it_names() {
    let regions = resolve(
        r#"region "corner" {
            area: (0%, 0%, 12.5%, 20%)
            instances: [ring, core, core.t]
        }"#,
    )
    .unwrap();
    assert_eq!(regions.len(), 1);
    assert_eq!(regions[0].name, "corner");
    assert_eq!(regions[0].area, (0.0, 0.0, 12.5, 20.0));
    let members = &regions[0].members;
    assert!(matches!(&members[0],
        FloorplanMember::Signal { instance, signal, .. } if instance == "top" && signal == "ring"));
    assert_eq!(members[1], FloorplanMember::Instance("top.core".into()));
    assert!(matches!(&members[2],
        FloorplanMember::Signal { instance, signal, .. } if instance == "top.core" && signal == "t"));
}

#[test]
fn a_name_the_design_lacks_is_an_error() {
    let err = resolve(
        r#"region "r" {
            area: (0%, 0%, 10%, 10%)
            instances: [rnig]
        }"#,
    )
    .unwrap_err();
    assert!(err.contains("rnig"), "{err}");
}

#[test]
fn an_area_needs_percentages_in_order() {
    for (area, says) in [
        ("(0, 0, 10, 10)", "needs a unit"),
        ("(0%, 0%, 10%)", "4 values"),
        ("(20%, 0%, 10%, 10%)", "low corner first"),
        ("(0%, 0%, 110%, 10%)", "0% to 100%"),
    ] {
        let src = source(&format!(
            r#"region "r" {{ area: {area}
                instances: [ring] }}"#
        ));
        let err = parse_and_build_hir(&src)
            .map(|hir| {
                hir.global_constraints
                    .iter()
                    .filter(|c| matches!(c, GlobalConstraint::Floorplan(_)))
                    .count()
            })
            .unwrap_err()
            .to_string();
        assert!(err.contains(says), "{area}: {err}");
    }
}
