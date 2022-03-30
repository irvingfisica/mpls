use chrono::prelude::*;
use serde::{Serialize, Deserialize};
use std::collections::{HashMap,BTreeMap};
use std::error::Error;
use calamine::{Xlsx,Reader,RangeDeserializerBuilder,open_workbook};
use svg::Document;
use svg::node::element::*;

use turambar::paths;
use turambar::escalas;
use turambar::svgplot;
use turambar::axis;
use turambar::transforms;

use svgplot::Plot;
use escalas::LinearScale;
use axis::Direccion;
use axis::Axis;
use transforms::get_translation;
use transforms::get_rotation;
use paths::simple_line;
use paths::simple_polygon;
use escalas::ContinuousScale;
use transforms::to_pxs;

pub fn get_sucursales(path: &str) -> Result<HashMap<usize,Sucursal>,Box<dyn Error>> {
    
    let mut mapa = HashMap::new();
    
    let mut sucraw: Xlsx<_> = open_workbook(path)?;
    let sucrange = sucraw.worksheets();

    let iter_sucs = RangeDeserializerBuilder::new().from_range::<_, Sucursal>(&sucrange[0].1)?;

    for registro in iter_sucs {
        match registro {
            Ok(sucursal) => {
                // sucursal.datos = Some(BTreeMap::new());
                mapa.insert(sucursal.id,sucursal);
            },
            Err(_) => continue
        }
    };

    Ok(mapa)
}

pub fn add_datos(sucursales: &mut HashMap<usize,Sucursal>, path: &str) -> Result<(),Box<dyn Error>> {

    let mut datraw: Xlsx<_> = open_workbook(path)?;
    let datrange = datraw.worksheets();

    let iter_dats = RangeDeserializerBuilder::new().from_range::<_, Datos>(&datrange[0].1)?;

    for registro in iter_dats {
        match registro {
            Ok(mut datos) => {
                match sucursales.get_mut(&datos.id) {
                    Some(sucursal) => {
                        datos.consolidar_fecha();
                        match datos.fecha {
                            Some(fecha) => {
                                match sucursal.datos.as_mut() {
                                    Some(data) => {data.insert(fecha,datos);},
                                    None => {
                                        let mut tmap = BTreeMap::new();
                                        tmap.insert(fecha,datos);
                                        sucursal.datos = Some(tmap);
                                    }
                                }
                                ;
                            },
                            None => continue
                        }
                        
                    },
                    None => continue
                }
                
            },
            _ => continue
        }
    };

    for (_,value) in sucursales.iter_mut() {

        match (value.fecha_apertura.as_mut(),value.datos.as_mut()) {
            (Some(fecha),Some(datos)) => {
                for (_,d) in datos.iter_mut() {
                    d.desde_fecha(*fecha);
                };
            },
            (None,Some(datos)) => {
                let mut iter = datos.iter_mut();
                let fecha = match iter.next() {
                    Some((_,dato)) => match dato.fecha {
                        Some(fecha) => {
                            let nfecha = NaiveDate::from_ymd(fecha.year(), fecha.month(), 1);
                            // value.fecha_apertura = Some(nfecha);
                            dato.desde_fecha(nfecha);
                            nfecha
                        },
                        None => continue
                    },
                    None => continue
                };

                for (_,d) in iter {
                    d.desde_fecha(fecha);
                }
            },
            _ => continue
        }

    };

    Ok(())

}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Sucursal {
    #[serde(alias = "Sucursal")]
    pub id: usize,
    #[serde(alias = "Nombre")]
    pub nombre: Option<String>,
    #[serde(alias = "Calle")]
    pub direccion: Option<String>,
    #[serde(alias = "CP")]
    pub cp: Option<String>,
    #[serde(alias = "Colonia")]
    pub colonia: Option<String>,
    #[serde(alias = "Delegación / Municipio")]
    pub municipio: Option<String>,
    #[serde(alias = "Estado")]
    pub estado: Option<String>,
    #[serde(alias = "Horario L-V")]
    pub horario_lv: Option<String>,
    #[serde(alias = "Horario S")]
    pub horario_s: Option<String>,
    #[serde(alias = "Horario D")]
    pub horario_d: Option<String>,
    #[serde(alias = "Tipo")]
    #[serde(deserialize_with = "de_tipo")]
    pub tipo: Tipo,
    #[serde(alias = "Ramos")]
    pub ramos: Option<String>,
    #[serde(alias = "Estatus")]
    #[serde(deserialize_with = "de_estatus")]
    pub estatus: Estatus,
    #[serde(alias = "X")]
    pub lon: Option<f64>,
    #[serde(alias = "Y")]
    pub lat: Option<f64>,
    #[serde(alias = "Fecha Apertura")]
    #[serde(deserialize_with = "de_opt_date")]
    pub fecha_apertura: Option<NaiveDate>,
    pub datos: Option<BTreeMap<NaiveDate,Datos>>
}

impl Sucursal {

    pub fn pixls_to_vec(&self, tipo: &TipoMargen, cadena: &mut Vec<Pixl>) {
        match &self.datos {
            Some(data) => {
                data.values()
                .for_each(|ele| {
                    match ele.to_pixl(tipo) {
                        Some(pixl) => cadena.push(pixl),
                        None => {}
                    }
                })
            },
            None => {}
        }
    }

    pub fn vec_of_empty_pixls(&self, tipo: &TipoMargen) -> Option<Vec<Pixl>> {
        let mut salida = Vec::new();

        self.pixls_to_vec(tipo, &mut salida);

        if salida.is_empty() {
            None
        } else {
            salida.sort_by(|a, b| a.meses.partial_cmp(&b.meses).unwrap());
            Some(salida)
        }
    }

}


#[derive(Debug,Serialize,Deserialize,Clone)]
pub enum Tipo {
    Sucursal,
    Desconocido,
}

#[derive(Debug,Serialize,Deserialize,Clone)]
pub enum Estatus {
    Abierta,
    Cerrada,
    Desconocido,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Datos {
    #[serde(alias = "ID Sucursal")]
    #[serde(deserialize_with = "de_sucursal")]
    id: usize,
    #[serde(alias = "Cartera")]
    cartera: Option<f64>,
    #[serde(alias = "Ingresos")]
    ingresos: Option<f64>,
    #[serde(alias = "Ingreso Venta")]
    ingresos_tie: Option<f64>,
    #[serde(alias = "Gastos Empeño")]
    gastos_emp: Option<f64>,
    #[serde(alias = "Gastos Tiendas")]
    gastos_tie: Option<f64>,
    #[serde(alias = "Gasto Total")]
    gasto_total: Option<f64>,
    #[serde(alias = "Margen Empeño")]
    pub margen_emp: Option<f64>,
    #[serde(alias = "Margen Venta")]
    pub margen_tie: Option<f64>,
    #[serde(alias = "Margen Final")]
    pub margen_final: Option<f64>,
    #[serde(alias = "Año")]
    year: Option<String>,
    #[serde(alias = "Mes")]
    #[serde(deserialize_with = "de_mes")]
    month: Option<String>,
    fecha: Option<NaiveDate>,
    pub dias_desde_apertura: Option<i64>,
    pub meses_desde_apertura: Option<f64>,
}

impl Datos {
    pub fn consolidar_fecha(&mut self) {
        match (self.month.as_ref(),self.year.as_ref()) {
            (Some(mes),Some(year)) => {
                match (mes.parse::<u32>(),year.parse::<i32>()) {
                    (Ok(m),Ok(y)) => {
                        self.fecha = Some(NaiveDate::from_ymd(y, m, last_day_of_month(y, m)));
                    },
                    _ => self.fecha = None
                }
            },
            _ => {
                self.fecha = None
            }
        };
    }

    pub fn desde_fecha(&mut self, fecha: NaiveDate) {
        match self.fecha {
            Some(sfecha) => {
                let dias = (sfecha - fecha).num_days();
                self.dias_desde_apertura = Some(dias);
                self.meses_desde_apertura = Some(dias as f64 / 30.0)
            },
            _ => {}
        };
    }

    pub fn to_pixl(&self, tipo: &TipoMargen) -> Option<Pixl> {
        match (self.meses_desde_apertura,self.fecha) {
            (Some(meses),Some(fecha)) => {
                let margen = match tipo {
                    TipoMargen::Empeño => self.margen_emp,
                    TipoMargen::Tienda => match self.margen_tie{
                        Some(margen) => if margen == 0.0 {
                            None
                        } else {
                            Some(margen)
                        },
                        None => None
                        
                    },
                    TipoMargen::Total => self.margen_final,
                };

                let pixl = Pixl {
                    sucursal: self.id,
                    fecha: fecha,
                    meses: meses,
                    margen: margen,
                    mean: None,
                    std: None,
                };
                Some(pixl)
            },
            _ => None
        }
    }
}

fn de_opt_date<'de, D>(deserializer: D) -> Result<Option<NaiveDate>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let data_type = calamine::DataType::deserialize(deserializer);
    match data_type {
        Ok(valor) => {
            match valor.as_date() {
                Some(fecha) => {
                    if fecha.year() < 1901 {
                        Ok(None)
                    } else {
                        Ok(Some(fecha))
                    }
                },
                None => Ok(None)
            }
        },
        _ => Ok(None),
    }
}

fn de_tipo<'de, D>(deserializer: D) -> Result<Tipo, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let data_type = calamine::DataType::deserialize(deserializer);
    match data_type {
        Ok(calamine::DataType::String(cadena)) => {
            if cadena == "Sucursal" {
                Ok(Tipo::Sucursal)
            } else {
                Ok(Tipo::Desconocido)
            }
        },
        _ => Ok(Tipo::Desconocido),
    }
}

fn de_estatus<'de, D>(deserializer: D) -> Result<Estatus, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let data_type = calamine::DataType::deserialize(deserializer);
    match data_type {
        Ok(calamine::DataType::String(cadena)) => {
            if cadena == "Abierta" {
                Ok(Estatus::Abierta)
            } else if cadena == "Cerrada" {
                Ok(Estatus::Cerrada)
            } else {
                Ok(Estatus::Desconocido)
            }
        },
        _ => Ok(Estatus::Desconocido),
    }
}

fn de_sucursal<'de, D>(deserializer: D) -> Result<usize, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let data_type = calamine::DataType::deserialize(deserializer);
    match data_type {
        Ok(calamine::DataType::Int(i)) => Ok(i as usize + 1000),
        Ok(calamine::DataType::Float(i)) => Ok(i as usize + 1000),
        _ => Ok(0),
    }
}

fn de_mes<'de, D>(deserializer: D) -> Result<Option<String>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let data_type = calamine::DataType::deserialize(deserializer);
    match data_type {
        Ok(calamine::DataType::Int(i)) => Ok(Some(format!("{:0>2}", i))),
        Ok(calamine::DataType::Float(i)) => Ok(Some(format!("{:0>2}", i as usize))),
        _ => Ok(None),
    }
}

fn last_day_of_month(year: i32, month: u32) -> u32 {
    NaiveDate::from_ymd_opt(year, month + 1, 1).unwrap_or(NaiveDate::from_ymd(year + 1, 1, 1)).pred().day()
}


pub fn metric_space<S,T>(stats: &Vec<(f64,f64,f64,f64)>, scx: &S, scy: &T) 
-> Result<Group, Box<dyn Error>> 
where 
    S: ContinuousScale,
    T: ContinuousScale,
{

    let mut g = Group::new();

    let bandas = bands(&stats, scx, scy)?;
    g = g.add(bandas);

    let promedios = stats.iter().map(|ele| (ele.0,ele.2)).collect();

    match simple_line(None, &promedios, scx, scy) {
        Some(pathm) => {
            let lineam = Path::new()
                .set("fill","none")
                .set("stroke-width",0.5)
                .set("stroke","black")
                .set("stroke-dasharray","4")
                .set("clip-path","url(#marco)")
                .set("d",pathm);

            g = g.add(lineam);
        },
        None => {}
    }

    let mut mg = Group::new();

    for (x,y,_,_) in stats.iter() {

        let (x,y) = match (scx.scale(*x),scy.scale(*y)) {
            (Some(xv),Some(yv)) => (xv,yv),
            _ => continue
        };

        let circle = Circle::new()
                    .set("cx", x)
                    .set("cy", y)
                    .set("r", 2)
                    .set("stroke-width",0.5)
                    .set("fill","gray")
                    .set("fill-opacity",0.1)
                    .set("stroke-opacity",0.3)
                    .set("clip-path","url(#marco)")
                    .set("stroke", "gray");

        mg = mg.add(circle);
    };
    g = g.add(mg);

    Ok(g)
}

pub fn consolidate_pixls(pixls: &mut Vec<Pixl>, wsize: i32) -> Result<(), Box<dyn Error>> {
    pixls.sort_by(|a, b| a.meses.partial_cmp(&b.meses).unwrap());

    let ln = pixls.len() as isize;
    let whalf = wsize as f64 / 2.0;
    let wd = whalf.floor() as isize;
    let wu = whalf.ceil() as isize;

    for i in 0..ln {
        let min = (i as isize - wd).max(0) as usize;
        let max = (i as isize + wu).min(ln) as usize;

        let filt: Vec<f64> = pixls[min..max].iter().filter_map(|pl| {
            match pl.margen {
                Some(margen) => Some(margen),
                None => None
            }
        }).collect();

        if filt.len() > 1 {
            let lensl = filt.len() as f64;

            let suma = filt.iter().fold(0.0, |acc, x| acc + x);
            let mean = suma / lensl;
            let sstd = filt.iter().fold(0.0, |acc, x| acc + (x - mean).powi(2));
            let std = (sstd/(lensl - 1.0)).sqrt();

            pixls[i as usize].mean = Some(mean);
            pixls[i as usize].std = Some(std);
        } else if filt.len() == 1 {
            let lensl = filt.len() as f64;

            let suma = filt.iter().fold(0.0, |acc, x| acc + x);
            let mean = suma / lensl;

            pixls[i as usize].mean = Some(mean);
            pixls[i as usize].std = None;
        } else {
            pixls[i as usize].mean = None;
            pixls[i as usize].std = None;
        };
    };

    Ok(())
}

pub fn urovoro(ida: &Vec<(f64,f64,f64,f64)>, mult1: f64, mult2: f64) -> Vec<(f64,f64)> {
    let vue: Vec<(f64,f64,f64,f64)> = ida.iter().rev().map(|ele| (ele.0,ele.1,ele.2,ele.3)).collect();

    let mut parte1: Vec<(f64,f64)> = ida.iter().map(|ele| (ele.0,ele.2 + ele.3 * mult1)).collect();
    let mut parte2: Vec<(f64,f64)> = vue.iter().map(|ele| (ele.0,ele.2 + ele.3 * mult2)).collect();

    parte1.append(&mut parte2);

    parte1
}

pub fn bands<T, S>(ida: &Vec<(f64,f64,f64,f64)>, scx: &T, scy: &S) 
-> Result<Group, Box<dyn Error>> 
where 
    S: ContinuousScale,
    T: ContinuousScale,
{
    let mut g = Group::new();

    let colband = ["#a50026","#d73027","#f46d43","#fdae61","#fee090",
                   "#e0f3f8","#abd9e9","#74add1","#4575b4","#313695"];

    for i in 0..10 {
        let pdata = urovoro(&ida, (i as f64 - 5.0) * 0.5, (i as f64 - 4.0) * 0.5);

        match simple_polygon(None, &pdata, scx, scy) {
            Some(ppath) => {
                let pdraw = Path::new()
                .set("fill",colband[i])
                .set("opacity",0.5)
                .set("stroke","none")
                .set("clip-path","url(#marco)")
                .set("d",ppath);

                g = g.add(pdraw);
            },
            None => continue
        };
    };

    Ok(g)

}

#[derive(Debug, Serialize)]
pub struct Pixl {
    sucursal: usize,
    fecha: NaiveDate,
    meses: f64,
    margen: Option<f64>,
    mean: Option<f64>,
    std: Option<f64>,
}

impl Pixl {
    pub fn get_quad(&self) -> Option<Quadata> {
        match (self.margen,self.mean,self.std) {
            (Some(margen), Some(mean), Some(std)) => {
                let quad = Quadata {
                    meses: self.meses,
                    valor: margen,
                    mean: mean,
                    std: std
                };
                Some(quad)
            },
            _ => None
        }
    }

    pub fn get_coord(&self) -> Option<(f64,f64)> {
        match self.margen {
            Some(margen) => Some((self.meses,margen)),
            None => None
        }
    }

    pub fn get_coord_date(&self) -> Option<(f64,f64,NaiveDate)> {
        match self.margen {
            Some(margen) => Some((self.meses,margen,self.fecha)),
            None => None
        }
    }
}

pub enum TipoMargen {
    Empeño,
    Tienda,
    Total,
}

#[derive(Debug)]
pub struct Quadata {
    meses: f64,
    valor: f64,
    mean: f64,
    std: f64
}

pub fn pixls_to_stats(margenes: &Vec<Pixl>) -> Vec<(f64,f64,f64,f64)> {
    let mut salida = Vec::new();

    for margen in margenes.iter() {
        match margen.get_quad() {
            Some(quad) => {salida.push((quad.meses,quad.valor,quad.mean,quad.std))},
            None => {}
        }
    };
    salida
}

pub fn pixls_to_coords(margenes: &Vec<Pixl>) -> Vec<(f64,f64)> {

    let mut salida = Vec::new();

    for margen in margenes.iter() {
        match margen.get_coord() {
            Some(tup) => salida.push(tup),
            None => {}
        };
    };
    salida
}

pub fn pixls_to_coords_date(margenes: &Vec<Pixl>) -> Vec<(f64,f64,NaiveDate)> {

    let mut salida = Vec::new();

    for margen in margenes.iter() {
        match margen.get_coord_date() {
            Some(tup) => salida.push(tup),
            None => {}
        };
    };
    salida
}

pub fn plot_line<S,T>(coords: &Vec<(f64,f64)>, scx: &S, scy: &T) -> Result<Group, Box<dyn Error>> 
    where 
        S: ContinuousScale,
        T: ContinuousScale,
    {

        let mut g = Group::new();

        let coords: Vec<(f64,f64)> = coords.iter().map(|(vd,vm)| (*vd,*vm)).collect();

        match simple_line(None, &coords, scx, scy) {
            Some(path) => {
                let linea = Path::new()
                .set("fill","none")
                .set("stroke-width",2)
                .set("stroke","black")
                .set("clip-path","url(#marco)")
                .set("d",path);

                g = g.add(linea);
            },
            None => {}
        }

        for (x,y) in coords.iter() {

            let (x,y) = match (scx.scale(*x),scy.scale(*y)) {
                (Some(xv),Some(yv)) => (xv,yv),
                _ => continue
            };

            let circle = Circle::new()
                        .set("cx", x)
                        .set("cy", y)
                        .set("r", 3.5)
                        .set("stroke-width",2)
                        .set("fill","white")
                        .set("clip-path","url(#marco)")
                        .set("stroke", "black");
    
            g = g.add(circle);
        };

        Ok(g)

}

pub fn plot_labels<S,T>(coords: &Vec<(f64,f64,NaiveDate)>, scx: &S, scy: &T) -> Result<Group, Box<dyn Error>> 
    where 
        S: ContinuousScale,
        T: ContinuousScale,
    {

        let mut g = Group::new();

        let coords: Vec<(f64,f64,NaiveDate)> = coords.iter().map(|(vd,vm,fecha)| (*vd,*vm,*fecha)).collect();

        for (x,y,fecha) in coords.iter() {

            let ymin = scy.extent().0;
            let fechacad = fecha.format("%m - %Y").to_string();

            let (x,y,ymin) = match (scx.scale(*x),scy.scale(*y),scy.scale(ymin)) {
                (Some(xv),Some(yv),Some(ymv)) => (xv,yv,ymv),
                _ => continue
            };

            let line = Line::new()
                        .set("x1", x)
                        .set("x2",x)
                        .set("y1",y)
                        .set("y2",ymin)
                        .set("stroke-width",0.3)
                        .set("stroke","black")
                        .set("stroke-dasharray","2")
                        .set("clip-path","url(#marco)");

            g = g.add(line);

            let rotation = get_rotation(-90.0);
            let translat = get_translation(x,ymin);
            let transf = format!("{} {}",translat,rotation);

            let mut gtext = Group::new()
                                    .set("transform",transf);

            let text = Text::new()
                        .set("x",0.0)
                        .set("y",0.0)
                        .set("dy",to_pxs(-3.0))
                        .set("dx",to_pxs(5.0))
                        .set("fill","gray")
                        .set("font-size",to_pxs(10.0))
                        .add(svg::node::Text::new(fechacad));

            gtext = gtext.add(text);
            g = g.add(gtext);

        };

        Ok(g)

}

pub fn pixl_plot(pixels: &Vec<Pixl>, sucpixl: Option<Vec<Pixl>>, titulo: Option<&String>,tipy: &str) -> Result<Document, Box<dyn Error>> {

    let all_stats = pixls_to_stats(pixels);

    let mut plot = Plot::new();
    plot.set_margin((40.0,20.0,40.0,100.0));
    plot.set_height(750.0);
    plot.set_width(1000.0);
    let mut mg = plot.get_tgroup();
    let mut docu = plot.get_docu();

    docu = docu.set("width",plot.width)
                .set("height",plot.height);

    let mut clip = ClipPath::new()
                    .set("id","marco");

    let fondo = Rectangle::new()
                    .set("x",0.0)
                    .set("y",0.0)
                    .set("width", plot.ef_w())
                    .set("height", plot.ef_h());

    clip = clip.add(fondo);
    mg = mg.add(clip);

    match titulo {
        Some(tit) => {
            let titlab = Text::new()
                    .set("x",plot.ef_w())
                    .set("y",0.0)
                    .set("text-anchor","end")
                    .set("font-size",to_pxs(18.0))
                    .add(svg::node::Text::new(tit));

            mg = mg.add(titlab);

        },
        _ => {}
    };

    let xlab = Text::new()
                    .set("x",plot.ef_w()*0.5)
                    .set("y",plot.height*0.945)
                    .set("text-anchor","middle")
                    .set("font-size",to_pxs(16.0))
                    .add(svg::node::Text::new("Meses desde apertura"));

    mg = mg.add(xlab);

    let rotation = get_rotation(-90.0);
    let translat = get_translation(0.0 - plot.width*0.07,plot.ef_h()*0.5);
    let transf = format!("{} {}",translat,rotation);

    let mut gtext = Group::new()
                            .set("transform",transf);

    let ylab = Text::new()
                    .set("x",0.0)
                    .set("y",0.0)
                    .set("text-anchor","middle")
                    .set("font-size",to_pxs(16.0))
                    .add(svg::node::Text::new(format!("Margen{}",tipy)));

    gtext = gtext.add(ylab);
    mg = mg.add(gtext);

    let mut scx = LinearScale::new();
        scx.range(0.0,plot.ef_w());

    let mut scy = LinearScale::new();
        scy.range(plot.ef_h(),0.0);

    let stats_suc_date = match sucpixl {
        Some(pixlvec) => {
                    let suc_stats = pixls_to_coords_date(&pixlvec);
                    Some(suc_stats)
                },
                None => None
    };

    let stats_suc = match stats_suc_date.as_ref() {
        Some(pixlvec) => {
            let coords: Vec<(f64,f64)> = pixlvec.iter()
                                    .map(|(x,y,_)| (*x,*y))
                                    .collect();
            Some(coords)
        },
        None => None
    };

    let (xmin,xmax) = match stats_suc.as_ref() {
        Some(suc_stats) => {

            let margen = 1.1;
            let eventos = suc_stats.len();

            let min = suc_stats.iter().min_by(|a,b| a.0.partial_cmp(&b.0).unwrap())
                            .ok_or("No hay un mínimo en sucursal")?.0;
            let max = suc_stats.iter().max_by(|a,b| a.0.partial_cmp(&b.0).unwrap())
                            .ok_or("No hay un máximo en sucursal")?.0;
            let rango = max - min;
            let rebanada = rango / eventos as f64;
            (min - rebanada * margen,max + rebanada * margen)
        },
        None => {
            let min = all_stats.iter().min_by(|a,b| a.0.partial_cmp(&b.0).unwrap())
                            .ok_or("No hay un mínimo en todas")?.0;
            let max = all_stats.iter().max_by(|a,b| a.0.partial_cmp(&b.0).unwrap())
                            .ok_or("No hay un máximo en todas")?.0;
            (min,max)
        }
    };

    let (ymin,ymax) = match stats_suc.as_ref() {
        Some(suc_stats) => {

            let margen = 0.1;

            let smin = suc_stats.iter().min_by(|a,b| a.1.partial_cmp(&b.1).unwrap())
                            .ok_or("No hay un mínimo en sucursal")?.1;
            let smax = suc_stats.iter().max_by(|a,b| a.1.partial_cmp(&b.1).unwrap())
                            .ok_or("No hay un máximo en sucursal")?.1;

            let filtrada: Vec<(f64,f64,f64,f64)> = all_stats.iter().filter(|(x,_,_,_)| (*x >= xmin) && (*x <= xmax))
                            .map(|ele| (ele.0,ele.1,ele.2,ele.3)).collect();

            let (min, max) = if !filtrada.is_empty() {
                let mint = filtrada.iter().min_by(|a,b| (a.2 - 2.5*a.3).partial_cmp(&(b.2 - 2.5*b.3)).unwrap())
                            .ok_or("No hay un mínimo en todas")?;
                let maxt = filtrada.iter().max_by(|a,b| (a.2 + 2.5*a.3).partial_cmp(&(b.2 + 2.5*b.3)).unwrap())
                            .ok_or("No hay un máximo en todas")?;

                let min = (mint.2 - 2.5*mint.3).min(smin);
                let max = (maxt.2 + 2.5*maxt.3).max(smax);

                (min,max)
            } else {
                (smin,smax)
            };

            let rango = max - min;
            (min - rango * margen,max + rango * margen)
        },
        None => {

            let margen = 0.1;

            let mint = all_stats.iter().min_by(|a,b| (a.2 - 2.5*a.3).partial_cmp(&(b.2 - 2.5*b.3)).unwrap())
                            .ok_or("No hay un mínimo")?;
            let maxt = all_stats.iter().max_by(|a,b| (a.2 + 2.5*a.3).partial_cmp(&(b.2 + 2.5*b.3)).unwrap())
                            .ok_or("No hay un máximo")?;

            let min = mint.2 - 2.5*mint.3;
            let max = maxt.2 + 2.5*maxt.3;

            let rango = max - min;
            (min - rango * margen,max + rango * margen)
        }
    };

    scx.domain(xmin.max(0.0),xmax);
    scy.domain(ymin,ymax);

    let xext = scx.extent();
    let yext = scy.extent();

    if xext.0 == xext.1 {
        return Err(From::from("La escala en x está indeterminada"))
    };

    if yext.0 == yext.1 {
        return Err(From::from("La escala en y está indeterminada"))
    };

    let mspace = metric_space(&all_stats, &scx, &scy)?;
    mg = mg.add(mspace);

    match stats_suc_date {
        Some(stats) => {

            let gl = plot_labels(&stats, &scx, &scy)?;
            mg = mg.add(gl);

        },
        None => {}
    };

    match stats_suc {
        Some(stats) => {

            let g = plot_line(&stats, &scx, &scy)?;
            mg = mg.add(g);
        },
        None => {}
    };

    let ejex: Axis = Axis::new(Direccion::Bottom, &scx, 10, 12.0);
    let mut gx = ejex.consolidar();

    let trans = get_translation(0.0, plot.ef_h());
    gx = gx.set("transform",trans);

    let ejey: Axis = Axis::new(Direccion::Left, &scy, 10, 12.0);
    let mut gy = ejey.consolidar();

    let trans = get_translation(0.0, 0.0);
    gy = gy.set("transform",trans);

    mg = mg.add(gx);
    mg = mg.add(gy);

    docu = docu.add(mg);

    Ok(docu)

}