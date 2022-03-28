use mpls::datos;
use datos::{get_sucursales,add_datos};
use std::io::Write;
use std::fs;
use fs::File;

fn main() {
    let path_sucursales = "./datos/Sucursales MLS.xlsx";
    let path_datos = "./datos/INDICADORES-SUC-ENE22.xlsx";

    let ruta_out = "./datos_procesados/sucursales_margen.json";

    let mut sucursales = get_sucursales(path_sucursales).unwrap();
    add_datos(&mut sucursales, path_datos).unwrap();

    let mut salida = File::create(ruta_out).unwrap();
    let j = serde_json::to_string_pretty(&sucursales).unwrap();
    write!(salida, "{}", j).unwrap();

}


