# Comando `git show`

El comando `git show` se utiliza para mostrar información detallada sobre un commit específico en un repositorio de Git. Proporciona detalles como el autor, la fecha, el mensaje del commit y los cambios realizados en los archivos.

## Sintaxis

```bash
git show [opciones] <commit>
```

## Uso común

### Mostrar el último commit

Para mostrar el último commit en la rama actual, utiliza:

```bash
git show
```

### Mostrar un commit específico

Si deseas ver un commit específico, puedes usar su hash. Por ejemplo:

```bash
git show <hash_del_commit>
```

### Mostrar un commit con formato abreviado

Para mostrar un commit con un formato más compacto, puedes usar la opción `--oneline`:

```bash
git show --oneline <hash_del_commit>
```

### Mostrar solo los cambios en un commit

Para mostrar solo los cambios realizados en un commit específico, puedes usar la opción `--patch` o `-p`:

```bash
git show --patch <hash_del_commit>
```

### Mostrar un commit con estadísticas

Para mostrar un commit junto con estadísticas de los cambios (número de líneas añadidas y eliminadas), utiliza la opción `--stat`:

```bash
git show --stat <hash_del_commit>
```

### Mostrar un commit con diferencias de color

Para resaltar las diferencias con colores, puedes usar la opción `--color`:

```bash
git show --color <hash_del_commit>
```

### Mostrar un commit con un formato personalizado

Puedes personalizar la salida del comando `git show` utilizando la opción `--pretty=format:` seguida de una cadena de formato. Por ejemplo, para mostrar solo el autor y el mensaje del commit:

```bash
git show --pretty=format:"%an: %s" <hash_del_commit>
```

### Mostrar un commit con diferencias resumidas

Para mostrar un commit con un resumen de las diferencias, puedes usar la opción `--name-status`:

```bash
git show --name-status <hash_del_commit>
```

### Mostrar un commit con diferencias en un archivo específico

Si deseas ver los cambios realizados en un archivo específico dentro de un commit, puedes especificar el nombre del archivo al final del comando:

```bash
git show <hash_del_commit> -- nombre_del_archivo
```

### Mostrar un commit con diferencias en un rango de commits

Para mostrar los cambios realizados en un rango de commits, puedes usar la notación de rango. Por ejemplo, para ver los cambios entre dos commits específicos:

```bash
git show <hash_del_commit>..<hash_del_commit2>
```

### Mostrar un commit con diferencias en un rango de commits (alternativa)

Otra forma de ver los cambios entre dos commits es utilizando la notación de rango con `HEAD`. Por ejemplo, para ver los cambios entre el commit actual y el commit anterior:

```bash
git show HEAD~1..HEAD
```

### Mostrar un commit con diferencias en un rango de commits (últimos n commits)

Para ver los cambios realizados en los últimos n commits, puedes usar la notación `HEAD~n..HEAD`. Por ejemplo, para ver los cambios en los últimos 3 commits:

```bash
git show HEAD~3..HEAD
```

### Mostrar un commit con diferencias en un rango de commits (últimos n commits, alternativa)

Otra forma de ver los cambios en los últimos n commits es utilizando la opción `-n`. Por ejemplo, para ver los cambios en los últimos 3 commits:

```bash
git show -n 3
```

### Mostrar un commit con diferencias en un rango de commits (últimos n commits, alternativa 2)

Otra forma de ver los cambios en los últimos n commits es utilizando la opción `--since`. Por ejemplo, para ver los cambios realizados en los últimos 7 días:

```bash
git show --since="7 days ago"
```

### Mostrar un commit con diferencias en un rango de commits (últimos n commits, alternativa 3)

Otra forma de ver los cambios en los últimos n commits es utilizando la opción `--after`. Por ejemplo, para ver los cambios realizados después de una fecha específica:

```bash
git show --after="2023-01-01"
```

### Mostrar un commit con diferencias en un rango de commits (últimos n commits, alternativa 4)

Otra forma de ver los cambios en los últimos n commits es utilizando la opción `--before`. Por ejemplo, para ver los cambios realizados antes de una fecha específica:

```bash
git show --before="2023-01-01"
```

### Mostrar un commit con diferencias en un rango de commits (últimos n commits, alternativa 5)

Otra forma de ver los cambios en los últimos n commits es utilizando la opción `--since` junto con `--until`. Por ejemplo, para ver los cambios realizados entre dos fechas específicas:

```bash
git show --since="2023-01-01" --until="2023-01-31"
```

### Mostrar un commit con diferencias en un rango de commits (últimos n commits, alternativa 6)

Otra forma de ver los cambios en los últimos n commits es utilizando la opción `--grep` para buscar commits que contengan una palabra clave específica en el mensaje del commit. Por ejemplo, para ver los commits que contienen la palabra "bugfix":

```bash
git show --grep="bugfix"
```

### Mostrar un commit con diferencias en un rango de commits (últimos n commits, alternativa 7)

Otra forma de ver los cambios en los últimos n commits es utilizando la opción `--author` para buscar commits realizados por un autor específico. Por ejemplo, para ver los commits realizados por "Juan":

```bash
git show --author="Juan"
```

### Mostrar un commit con diferencias en un rango de commits (últimos n commits, alternativa 8)

Otra forma de ver los cambios en los últimos n commits es utilizando la opción `--committer` para buscar commits realizados por un committer específico. Por ejemplo, para ver los commits realizados por "Maria":

```bash
git show --committer="Maria"
```

### Mostrar un commit con diferencias en un rango de commits (últimos n commits, alternativa 9)

Otra forma de ver los cambios en los últimos n commits es utilizando la opción `--no-merges` para excluir los commits de merge. Por ejemplo, para ver los commits sin incluir los merges:

```bash
git show --no-merges
```

### Mostrar un commit con diferencias en un rango de commits (últimos n commits, alternativa 10)

Otra forma de ver los cambios en los últimos n commits es utilizando la opción `--merges` para incluir solo los commits de merge. Por ejemplo, para ver solo los commits de merge:

```bash
git show --merges
```

### Mostrar un commit con diferencias en un rango de commits (últimos n commits, alternativa 11)

Otra forma de ver los cambios en los últimos n commits es utilizando la opción `--reverse` para mostrar los commits en orden inverso. Por ejemplo, para ver los commits en orden inverso:

```bash
git show --reverse
```

### Mostrar un commit con diferencias en un rango de commits (últimos n commits, alternativa 12)

Otra forma de ver los cambios en los últimos n commits es utilizando la opción `--abbrev-commit` para mostrar solo una versión abreviada del hash del commit. Por ejemplo, para ver los commits con hashes abreviados:

```bash
git show --abbrev-commit
```

### Mostrar un commit con diferencias en un rango de commits (últimos n commits, alternativa 13)

Otra forma de ver los cambios en los últimos n commits es utilizando la opción `--relative-date` para mostrar las fechas de los commits en un formato relativo. Por ejemplo, para ver los commits con fechas relativas:

```bash
git show --relative-date
```

### Mostrar un commit con diferencias en un rango de commits (últimos n commits, alternativa 14)

Otra forma de ver los cambios en los últimos n commits es utilizando la opción `--date=short` para mostrar las fechas de los commits en un formato corto. Por ejemplo, para ver los commits con fechas en formato corto:

```bash
git show --date=short
```


Y existen muchas más opciones y combinaciones que puedes explorar en la documentación oficial de Git para personalizar la salida del comando `git show` según tus necesidades.

```bash
git show --help
```