use mpls::datos;
use datos::{get_sucursales,add_datos};
use datos::consolidate_pixls;
use datos::pixl_plot;
use datos::TipoMargen;
use std::collections::HashSet;
use csv::Writer;
use std::io::Write;
use std::fs;
use fs::File;


fn main() {
    let path_sucursales = "./datos/Sucursales MLS.xlsx";
    let path_datos = "./datos/INDICADORES-SUC-ENE22.xlsx";

    let ruta_out = "./datos_procesados/sucursales_margen.json";

    let afuera = HashSet::from([1290,1284,1280,1283,1001]);
    let numsuc = 1295;

    let mut sucursales = get_sucursales(path_sucursales).unwrap();
    add_datos(&mut sucursales, path_datos).unwrap();

    let mut salida = File::create(ruta_out).unwrap();
    let j = serde_json::to_string_pretty(&sucursales).unwrap();
    write!(salida, "{}", j).unwrap();

    let tipos = [TipoMargen::Empeño,TipoMargen::Tienda, TipoMargen::Total];

    for tipo in tipos {

        let tipcad = match tipo {
            TipoMargen::Empeño => "emp",
            TipoMargen::Tienda => "tie",
            TipoMargen::Total => "tot",
        };

        let mut wtr = Writer::from_path(format!("./pruebas/pl_{}.csv",tipcad)).unwrap();

        let mut margenes = Vec::new();

        let sucpixl = match sucursales.get(&numsuc) {
            Some(suc) => suc.vec_of_empty_pixls(&tipo),
            None => None
        };

        sucursales.values()
            .filter(|ele| {!afuera.contains(&ele.id)})
            .for_each(|ele| ele.pixls_to_vec(&tipo, &mut margenes));

        consolidate_pixls(&mut margenes, 120).unwrap();

        let plot = pixl_plot(&margenes, sucpixl).unwrap();

        svg::save(format!("./pruebas/pl_{}.svg",tipcad),&plot).unwrap();

        for margen in margenes.iter() {
            wtr.serialize(margen).unwrap()
        };

        wtr.flush().unwrap();
    }

}


