> [!WARNING]: El comando `git clean` eliminará permanentemente los archivos y directorios no trackeados. Incluyendo los archivos ignorados por `.gitignore`. Asegúrate de que realmente deseas eliminarlos antes de ejecutar el comando.

# Comando `git clean`

El comando `git clean` se utiliza para eliminar archivos **no trackeados** en el directorio de trabajo (WD). Estos archivos pueden ser archivos temporales, archivos generados por el sistema o cualquier archivo que no esté siendo gestionado por Git.

## Sintaxis

```bash
git clean [opciones]
```

## Uso común

### Vista previa de archivos a eliminar 

Antes de eliminar archivos, es recomendable hacer una vista previa de los archivos que serán eliminados. Puedes hacerlo con la opción `-n` o `--dry-run`:

```bash
git clean -n
```

Este comando mostrará una lista de archivos que serían eliminados sin realizar ninguna acción.

Si se quiere incluir directorios en la vista previa, utiliza la opción `-d`:

```bash
git clean -nd
```

### Eliminar archivos no trackeados

Para eliminar los archivos no trackeados, utiliza la opción `-f` (force):

```bash
git clean -f
```

### Eliminar directorios no trackeados

Si deseas eliminar también los directorios no trackeados, utiliza la opción `-d` junto con `-f`:

```bash
git clean -fd
```

### Elmiminacion de un archivo especifico

Si deseas eliminar un archivo específico que no está siendo trackeado, puedes especificar el nombre del archivo:

```bash
git clean -f nombre_del_archivo
```

