# Comando `git fetch`

El comando `git fetch` se utiliza para descargar cambios desde un repositorio remoto sin fusionarlos automáticamente con la rama actual. Esto permite revisar los cambios antes de integrarlos en el proyecto local.

## Sintaxis

```bash
git fetch [opciones] [repositorio] [rama]
```

## Uso común

### Descargar cambios desde el repositorio remoto por defecto

Para descargar los cambios desde el repositorio remoto por defecto (generalmente llamado `origin`), utiliza:

```bash
git fetch origin
```

Una manera más simple de hacerlo es:

```bash
git fetch
```

Esto es gracias a que `origin` es el nombre predeterminado para el repositorio remoto principal.

### Descargar cambios desde un repositorio remoto específico

Si tienes múltiples repositorios remotos configurados, puedes especificar el nombre del repositorio desde el cual deseas descargar los cambios. Por ejemplo, si tienes un repositorio remoto llamado `upstream`, utiliza:

```bash
git fetch upstream
```

### Descargar una rama específica

Para descargar una rama específica desde el repositorio remoto, puedes especificar el nombre de la rama. Por ejemplo, para descargar la rama `feature-branch` desde `origin`, utiliza:

```bash
git fetch origin feature-branch
```

### Descargar todas las ramas

Para descargar todas las ramas desde el repositorio remoto, utiliza la opción `--all`:

```bash
git fetch --all
```

### Descargar y eliminar referencias a ramas remotas eliminadas

Para eliminar las referencias a ramas remotas que ya no existen en el repositorio remoto, utiliza la opción `--prune`:

```bash
git fetch --prune
```

### Descargar y eliminar referencias a ramas remotas eliminadas de un repositorio específico

Si deseas eliminar las referencias a ramas remotas eliminadas de un repositorio específico, puedes combinar `--prune` con el nombre del repositorio. Por ejemplo, para hacerlo con `origin`, utiliza:

```bash
git fetch --prune origin
```