**GRUPO A - Proyecto DDS**
 
En este proyecto, se ha establecido como punto de partida, qué caracterísitcas son las principales para detectar que una web pueda ser vulnerada.
 
Fuente de datos-->http://www.malwaredomains.com/.
 
Definición del conjunto de los datos:
 
- **Domain**: dominio al cual pertenecen. 
- **Fecha**: fecha en el que se realizó el ataque.
- **Categoría**: tipo de página web (venta de comida, ropa, automóviles...) 
- **IP**
- **Proveedor**
- **Origen**: país de donde proceden.
- **Tipología servidor**
- **Autor dominio**
 
Objetivos principales: 
 
1. Relacionar el **grado** de **vulnerabilidad** de una **web** por su categoría. 
2. Relacionar qué **países** son los más **atacados**, en cuanto a webs, en cada mes del año-> rworldmap (hecho), falta juntar todos los datos (probado solo con algunas IP)
3. Qué **proveedores** suelen ser los **más vulnerables**. 
4. Establecer en qué **épocas del año** suelen haber **más phishings**.
 
Objetivo modelo estadístico: 
 
En base a las características comentadas anteriormente, definir qué probabilidad hay que una web pueda ser vulnerada, jugando con el porcentaje de casos extraido de las cuestiones anteriores. 
 