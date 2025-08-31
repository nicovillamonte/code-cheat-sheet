# Comando `git branch`

El comando `git branch` se utiliza para gestionar las ramas en un repositorio de Git. Permite crear, listar, renombrar y eliminar ramas, lo que es esencial para el flujo de trabajo en proyectos colaborativos.

## Sintaxis

```bash
git branch [opciones] [nombre_de_rama]
```

## Uso común

### Listar ramas

Para listar todas las ramas en el repositorio, utiliza:

```bash
git branch
```

### Crear una nueva rama

Para crear una nueva rama, utiliza:

```bash
git branch nombre_de_rama
```

### Cambiar a una rama específica

Para cambiar a una rama específica, utiliza:

```bash
git checkout nombre_de_rama
```

### Crear y cambiar a una nueva rama

Para crear y cambiar a una nueva rama en un solo paso, utiliza:

```bash
git checkout -b nombre_de_rama
```

### Renombrar una rama

Para renombrar la rama actual, utiliza:

```bash
git branch -m nuevo_nombre_de_rama
```

Si no estas en la rama que deseas renombrar, puedes especificar el nombre actual:

```bash
git branch -m nombre_actual nuevo_nombre_de_rama
```

### Eliminar una rama

Para eliminar una rama que ya no necesitas, utiliza:

```bash
git branch -d nombre_de_rama
```

### Forzar la eliminación de una rama

Si deseas eliminar una rama que no ha sido fusionada, puedes forzar la eliminación con:

```bash
git branch -D nombre_de_rama
```

### Ver ramas remotas

Para ver las ramas remotas, utiliza:

```bash
git branch -r
```

### Ver todas las ramas (locales y remotas)

Para ver todas las ramas, tanto locales como remotas, utiliza:

```bash
git branch -a
```