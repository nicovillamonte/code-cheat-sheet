# Comando `git grep`

El comando `git grep` se utiliza para buscar patrones específicos dentro del contenido de los archivos en un repositorio de Git. Es una herramienta poderosa para localizar rápidamente información en el código fuente.

## Sintaxis

```bash
git grep [opciones] <patrón> [<revisión>...]
```

## Uso común

### Buscar un patrón en el repositorio

Para buscar un patrón específico en todo el repositorio, utiliza:

```bash
git grep "patrón_de_búsqueda"
```

### Buscar un patrón en una rama específica

Para buscar un patrón en una rama específica, utiliza:

```bash
git grep "patrón_de_búsqueda" nombre_de_rama
```

### Buscar un patrón en un commit específico

Para buscar un patrón en un commit específico, utiliza su hash:

```bash
git grep "patrón_de_búsqueda" <hash_del_commit>
```

### Buscar un patrón en archivos con una extensión específica

Para buscar un patrón en archivos con una extensión específica, utiliza la opción `--` seguida del patrón de archivo. Por ejemplo, para buscar en archivos `.txt`:

```bash
git grep "patrón_de_búsqueda" -- *.txt
```

### Buscar un patrón y mostrar el número de línea

Para buscar un patrón y mostrar el número de línea donde se encuentra, utiliza la opción `-n`:

```bash
git grep -n "patrón_de_búsqueda"
```

### Buscar el número de ocurrencias de un patrón

Para buscar el número de ocurrencias de un patrón en el repositorio, utiliza la opción `-c`:

```bash
git grep -c "patrón_de_búsqueda"
```

### Buscar un patrón de forma recursiva

Para buscar un patrón de forma recursiva en todos los archivos del repositorio, utiliza la opción `-r`:

```bash
git grep -r "patrón_de_búsqueda"
```