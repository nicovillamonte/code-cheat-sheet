# Comando `git shortlog`

El comando `git shortlog` se utiliza para generar un resumen conciso de los commits en un repositorio de Git. Agrupa los commits por autor y muestra el número de commits realizados por cada autor, junto con los mensajes de commit.

## Sintaxis

```bash
git shortlog [opciones] [<rango_de_commits>]
```

## Uso común

### Resumen de commits por autor

Para generar un resumen de commits agrupados por autor, utiliza:

```bash
git shortlog
```

### Resumen de commits en un rango específico

Para generar un resumen de commits en un rango específico, puedes especificar el rango de commits. Por ejemplo, para ver los commits entre dos etiquetas:

```bash
git shortlog v1.0..v2.0
```

### Conteo de commits por autor

Para incluir estadísticas adicionales, como el número de líneas añadidas y eliminadas por cada autor, utiliza la opción `-s`:

```bash
git shortlog -s
```

### Conteo de commits por autor en orden descendente

Para ordenar los autores por el número de commits en orden descendente, utiliza la opción `-n` junto con `-s`:

```bash
git shortlog -s -n
```
