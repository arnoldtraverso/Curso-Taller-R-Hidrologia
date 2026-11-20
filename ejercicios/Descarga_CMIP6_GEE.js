//-----------------------------------------------------------------------------------
// DESCARGA DE DATOS NEX-GDDP-CMIP6 POR SUBCUENCA
// PRECIPITACION (mm/dia), TMAX y TMIN (°C) - FORMATO LARGO
// HISTORICO + ESCENARIO FUTURO, VARIOS MODELOS

// @AUTOR: KEVIN ARNOLD TRAVERSO
// MAIL: ARNOLD.TRAVERSO@GMAIL.COM
//-----------------------------------------------------------------------------------

// 1. CARGAR SUBCUENCAS
var cuenca = ee.FeatureCollection('projects/ee-arnoldtraverso/assets/Huancane_ejemplo');
var campoCodigo = 'Subbasin';  // nombre del campo con el código en el asset (1, 2, ..., 5)
// Si el campo no existe, la columna "Subcuenca" del CSV sale vacía.
// Para revisar los campos: print(cuenca.first());

Map.addLayer(cuenca, {color: 'blue'}, 'Subcuencas Huancane');
Map.centerObject(cuenca);

// 2. PARAMETROS
var modelos = [
  'ACCESS-CM2',
  'MIROC6',
  'MPI-ESM1-2-HR'
];
var escenario   = 'ssp245';     // 'ssp126', 'ssp245', 'ssp370', 'ssp585'
var fechaInicio = '1981-01-01'; // si es antes de 2015 incluye histórico
var fechaFin    = '2051-01-01'; // fecha final exclusiva

// Ver lista de modelos disponibles
// print('Modelos disponibles:', ee.ImageCollection('NASA/GDDP-CMIP6').aggregate_array('model').distinct());

// 3. CONVERSION DE UNIDADES
var convertir = function(img) {
  var pr   = img.select('pr').multiply(86400).rename('Pr_mm');
  var tmax = img.select('tasmax').subtract(273.15).rename('Tmax');
  var tmin = img.select('tasmin').subtract(273.15).rename('Tmin');
  return pr.addBands([tmax, tmin])
    .copyProperties(img, ['system:time_start', 'model', 'scenario']);
};

// 4. FUNCION DE EXTRACCION POR MODELO
var extraer = function(modelo) {
  var coleccion = ee.ImageCollection('NASA/GDDP-CMIP6')
    .filter(ee.Filter.eq('model', modelo))
    .filter(ee.Filter.inList('scenario', ['historical', escenario]))
    .filterDate(fechaInicio, fechaFin)
    .select(['pr', 'tasmax', 'tasmin'])
    .map(convertir);

  var serie = coleccion.map(function(img) {
    var fecha = img.date().format('YYYY-MM-dd');
    var stats = img.reduceRegions({
      collection: cuenca.select([campoCodigo]),
      reducer: ee.Reducer.mean(),
      scale: 1000
    });
    return stats.map(function(f) {
      return f.set({
        'Fecha': fecha,
        'Subcuenca': f.get(campoCodigo),
        'Modelo': img.get('model'),
        'Escenario': img.get('scenario')
      });
    });
  }).flatten();

  return {coleccion: coleccion, serie: serie};
};

// 5. EXPORTAR UNA TAREA POR MODELO
modelos.forEach(function(modelo) {
  var res = extraer(modelo);
  print(modelo + ' - N° de imágenes (días):', res.coleccion.size());

  Export.table.toDrive({
    collection: res.serie,
    description: 'CMIP6_' + modelo.replace(/-/g, '_') + '_' + escenario,
    folder: 'CMIP6',
    fileNamePrefix: 'Huancane_' + modelo + '_' + escenario,
    fileFormat: 'CSV',
    selectors: ['Fecha', 'Subcuenca', 'Modelo', 'Escenario', 'Pr_mm', 'Tmax', 'Tmin']
  });
});