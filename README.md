**RUTINA PARA LA CONSOLIDACIÓN DE REGISTROS BIOLÓGICOS DE COLOMBIA Y PAÍSES VECINOS**

  Por: Cristian Alexander Cruz Rodríguez

  Fecha: 31-07-2023
 
El presente documento describe las rutinas y procesamientos generados para obtener los registros biológicos tanto de Colombia, como de países vecinos a partir de la información disponible en portales Web, y en fuentes secundarias (colecciones del I. Humboldt, I2D y ANLA) para el set de datos (set16).

**Generalidades**

El proceso consta de 5 pasos. Las rutinas implementadas se ejecutan en el lenguaje de programación R (1_unify_archive_with_data, 2_set0, 3_set1, 4_set2, 5_set16) y Python (extract_specific_column_to_GBIF_for_set16.py). Estas rutinas son de libre acceso. 

**1.	Obtención de los datos originales (Unify_archive_with_data)**

Los datos obtenidos de portales web abiertos son obtenidos de GBIF, SpeciesLink y eBird. Tambien se documenta la incorporación de datos disponibles en repositors del Instituto Humboldt. Los datos crudos se almacenan en la carpeta “datos_originales”, en el directorio “ [Unidad de disco]:\Cristian_data\Humboldt\Set16” y las rutinas se encuentran en la carpeta scripts en la ruta “[Unidad de disco]:\Set16”.

**2.	Unificación de datos en matriz de datos del Set16 (2_Set 0)**

Los datos obtenidos en el numeral anterior deben ser unificados en una única matriz, de forma tal que se puedan limpiar y revisar en conjunto. Tambien se acotan que contengan sólo los registros que se encuentran en la extensión geográfica definida por BioModelos y por medo de la función cleanSciNames se elimina puntos, espacios y los nombres de autores de más de la columna que contiene el nombre científico. A este paso se le conoce como Set 0, y su procesamiento se realiza utilizando el script **_2_set0.r_**.

**3.	Verificación Taxonómica de los registros (Set 1)**

La unificación de los registros no mantiene la taxonomía superior de los registros, sino que sólo conserva el nombre origina de las especies. Es por ello que en este paso se realiza una búsqueda de dicha información y se incluye en el set de datos. También se verifica que las especies registradas sean válidas usando para ello el Backbone de GBIF, así como el listado de especies del Catalogo de la Vida. Este proceso se efectúa empleando el script **_3_set1_**.

**4. Eliminación de duplicados, especies en Colombia e incertidumbre taxonómica (Set 2)**

En la siguiente etapa se realiza una verificación del nivel de incertidumbre de la evaluación taxonómica, basado en los listados generados por el equipo en años anteriores. A su vez, conjugando los datos que identifican los registros biológicos y las coordenadas geográficas se evaluaron cuantos registros están duplicados. Finalmente, se evalúa que coordenadas se encontraban dentro de los limites político administrativos de Colombia. Para ello se creó una columna que incluía si se encontraba en el país (con el valor de cero), y con el número 1, cuando si estaban dentro del país.

Este proceso se efectúa empleando el script **_4_set2_**.

**5. Verificación de la información geográfica de los registros (Set3 al set16)**

Para verificar que información geográfica de los registros sea correcta, se llevan a cabo una serie de rutinas que identifican tanto la coincidencia en la información a nivel de país, como a nivel de departamentos y limites municipales. Este procedimiento se puede agrupar en cuatro grandes categorías que son descritas a continuación:

a.	Se eliminan los datos que no tienen coordenadas y se identifica el país que coincide con las coordenadas resultantes.
b.	 Se evalúa la conjunción entre la información asociada a los municipios y departamentos, y se propone uno basado en los límites político administrativos definidos para el país entre 1964 y el 2014.
c.	Se sugiere un departamento y un municipio para cada registro.
d.	Se evalúa que registros se encuentran duplicados, basado en una concatenación de coordenadas, ID y especie.

Este proceso se efectúa empleando el script **_5_set16_**. y como resultado final se obtiene un archivo en formato RData, que recopila el resultado obtenido a lo largo de todo el proceso. 

Al finalizar la unificación de registros en el set16, el compilado de datos cuenta con una serie de evaluaciones para identificar su nivel de incertidumbre. Lo anterior para que su filtrado permita establecer el mejor set de datos disponible, según las necesidades del usuario. Las  evaluaciones se realizan usando la herramienta CoordinateCleaner de Zizka y colaboradores (2019).
