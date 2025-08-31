# Comando `git diff`

El comando `git diff` se utiliza para mostrar las diferencias entre varios estados en un repositorio de Git. Permite comparar cambios entre el área de trabajo, el área de preparación (staging area) y los commits.


## Sintaxis

```bash
git diff [opciones] [<commit>] [<commit>] [--] [<ruta>...]
```

## Uso común

### Mostrar diferencias no staged

Para ver las diferencias entre el área de trabajo y el área de preparación, utiliza:

```bash
git diff
```

### Mostrar diferencias staged

Para ver las diferencias entre el área de preparación y el último commit, utiliza:

```bash
git diff --cached
```

### Mostrar diferencias entre dos commits
Para comparar dos commits específicos, utiliza sus hashes:

```bash
git diff <hash_del_commit1> <hash_del_commit2>
```

### Mostrar diferencias entre un commit y el área de trabajo

Para comparar un commit específico con el área de trabajo, utiliza:

```bash
git diff <hash_del_commit>
```

### Mostrar diferencias entre un commit y el área de preparación

Para comparar un commit específico con el área de preparación, utiliza:

```bash
git diff <hash_del_commit> --cached
```

### Mostrar diferencias para un archivo específico

Para ver las diferencias de un archivo específico, añade la ruta del archivo al final del comando:

```bash
git diff <hash_del_commit1> <hash_del_commit2> -- ruta/del/archivo
```

### Mostrar diferencias en un rango de commits

Para ver las diferencias entre un rango de commits, utiliza la notación de rango:

```bash
git diff <hash_del_commit1>..<hash_del_commit2>
```

### Mostrar diferencias en los últimos n commits

Para ver las diferencias en los últimos n commits, utiliza la notación `HEAD~n..HEAD`:

```bash
git diff HEAD~n..HEAD
```

### Mostrar diferencias con un formato resumido

Para ver un resumen de las diferencias, puedes usar la opción `--stat`:

```bash
git diff --stat <hash_del_commit1> <hash_del_commit2>
```

### Mostrar diferencias con un formato de parches

Para ver las diferencias en formato de parches, puedes usar la opción `--patch` o `-p`:

```bash
git diff -p <hash_del_commit1> <hash_del_commit2>
```

### Ignorar espacios en blanco

Para ignorar los cambios en espacios en blanco, utiliza la opción `-w`:

```bash
git diff -w <hash_del_commit1> <hash_del_commit2>
```

### Mostrar diferencias con colores

Para resaltar las diferencias con colores, puedes usar la opción `--color`:

```bash
git diff --color <hash_del_commit1> <hash_del_commit2>
```

### Mostrar diferencias en un formato personalizado

Puedes personalizar la salida del comando `git diff` utilizando la opción `--pretty=format:` seguida de una cadena de formato. Por ejemplo, para mostrar solo las líneas añadidas y eliminadas:

```bash
git diff --pretty=format:"%h - %an: %s" <hash_del_commit1> <hash_del_commit2>
```