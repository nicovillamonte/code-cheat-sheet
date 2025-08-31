# Comando `git log`

El comando `git log` se utiliza para mostrar el historial de commits en un repositorio de Git. Permite ver los cambios realizados, quién los hizo y cuándo.

## Uso Básico

1. Abre una terminal o línea de comandos en tu repositorio local.
2. Ejecuta el siguiente comando:

```bash
git log
```

Esto mostrará una lista de todos los commits en el repositorio, junto con información detallada sobre cada uno.

## Información proporcionada

Al ejecutar `git log`, verás una salida que incluye:

- **Hash del commit**: Un identificador único para cada commit.
- **Autor**: El nombre y correo electrónico del autor del commit.
- **Fecha**: La fecha y hora en que se realizó el commit.
- **Mensaje del commit**: Una descripción breve de los cambios realizados.

## Opciones Comunes

- `--oneline`: Muestra el historial de commits en una sola línea por commit.
- `--graph`: Muestra un gráfico ASCII del historial de commits.
- `--author="Nombre"`: Filtra los commits por autor.
- `--since="fecha"`: Muestra los commits realizados desde una fecha específica.
- `--until="fecha"`: Muestra los commits realizados hasta una fecha específica.

Estas opciones se pueden combinar para personalizar la salida del comando `git log` según tus necesidades.

Existen muchas más opciones y formatos disponibles para `git log`, que puedes explorar en la [documentación oficial de Git](https://git-scm.com/docs/git-log).