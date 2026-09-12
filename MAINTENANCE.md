# Estado de mantenimiento

rgisDR 0.3.1 se conserva como paquete anterior del ecosistema GeoDOM.
Su alcance y sus datos son los que describe esta versión; no representa una
actualización automática de la división territorial vigente.

Para desarrollos nuevos use [geodomR](https://github.com/GeoDOMProject/geodomR),
que descarga las fuentes públicas, permite elegir capas y ofrece la API `gd_*`.
Las aplicaciones existentes pueden seguir fijando la versión de rgisDR.
La transición requiere adaptar explícitamente los nombres de funciones y validar
los códigos territoriales; no se reemplazan silenciosamente las capas históricas.

La revisión 0.3.1 declara las dependencias requeridas (sfDR y ggplot2) y elimina
la instalación automática de paquetes al cargar el espacio de nombres.
