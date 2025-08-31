# Comando `git checkout`

El comando `git checkout` se utiliza para cambiar entre ramas o restaurar archivos en el directorio de trabajo. Es una herramienta esencial para navegar por el historial de commits y trabajar con diferentes versiones del código.

## Sintaxis

```bash
git checkout [opciones] <rama>
```

## Uso común

### Cambiar a una rama específica

Para cambiar a una rama específica, utiliza:

```bash
git checkout nombre_de_rama
```

### Crear y cambiar a una nueva rama

Para crear una nueva rama y cambiar a ella en un solo paso, utiliza:

```bash
git checkout -b nombre_de_nueva_rama
```

### Restaurar un archivo específico

Si deseas restaurar un archivo específico a su estado en el último commit, utiliza:

```bash
git checkout -- nombre_del_archivo
```

> Actualmente es mejor utilizar `git restore` para restaurar archivos.

### Cambiar a un commit específico

Para cambiar a un commit específico, puedes usar su hash. Por ejemplo:

```bash
git checkout <hash_del_commit>
```

### Cambiar a un commit anterior

Para cambiar a un commit anterior, puedes usar la notación `HEAD~n`, donde `n` es el número de commits hacia atrás. Por ejemplo, para cambiar al commit anterior:

```bash
git checkout HEAD~1
```

### Descartar cambios en el área de trabajo

Si deseas descartar todos los cambios en el área de trabajo y volver al último commit, utiliza:

```bash
git checkout .
```

### Volver al commit anterior del que estabas

Si deseas volver al commit anterior del que estabas, puedes usar:

```bash
git checkout -
```