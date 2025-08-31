# Comando `git tag`

El comando `git tag` se utiliza para crear, listar, eliminar y gestionar etiquetas (tags) en un repositorio de Git. Las etiquetas son referencias que apuntan a commits específicos y se utilizan comúnmente para marcar versiones importantes en el historial del proyecto.

## Sintaxis

```bash
git tag [nombre_tag] [commit]
```

## Uso común

### Listar todas las etiquetas

Para listar todas las etiquetas en el repositorio, utiliza:

```bash
git tag
```

### Crear una etiqueta ligera

Para crear una etiqueta ligera (lightweight tag), que es simplemente un puntero a un commit, utiliza:

```bash
git tag nombre_de_tag
```

### Crear una etiqueta anotada

Para crear una etiqueta anotada (annotated tag), que incluye información adicional como el nombre del autor, la fecha y un mensaje, utiliza:

```bash
git tag -a nombre_de_tag -m "Mensaje de la etiqueta"
```

### Crear una etiqueta en un commit específico

Si deseas crear una etiqueta en un commit específico, puedes proporcionar el hash del commit al final del comando. Por ejemplo:

```bash
git tag -a nombre_de_tag <hash_del_commit> -m "Mensaje de la etiqueta"
```

### Eliminar una etiqueta

Para eliminar una etiqueta, utiliza el siguiente comando:

```bash
git tag -d nombre_de_tag
```

### Compartir etiquetas con un repositorio remoto

Para compartir las etiquetas con un repositorio remoto, utiliza:

```bash
git push origin --tags
```

### Eliminar una etiqueta en un repositorio remoto

Para eliminar una etiqueta en un repositorio remoto, utiliza:

```bash
git push origin --delete nombre_de_tag
```

### Ver detalles de una etiqueta

Para ver los detalles de una etiqueta anotada, utiliza:

```bash
git show nombre_de_tag
```

## Moverse a un commit etiquetado

Para moverte a un commit etiquetado, puedes usar el comando `git checkout` seguido del nombre de la etiqueta:

```bash
git checkout nombre_de_tag
```

> [!NOTE]: Cambiar a una etiqueta te coloca en un estado de "detached HEAD", lo que significa que no estás en una rama. Si realizas cambios y deseas conservarlos, considera crear una nueva rama desde ese punto.