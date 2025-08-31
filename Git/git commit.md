# Comando `git commit`

El comando `git commit` se utiliza para guardar los cambios en el repositorio de Git. Cada commit crea un punto en el historial del proyecto, permitiendo rastrear y revertir cambios si es necesario.

## Uso Básico

1. Abre una terminal o línea de comandos en tu repositorio local.
2. Asegúrate de que los archivos que deseas confirmar estén en el área de preparación (staging area). Puedes verificar esto con el comando:

```bash
git status
```

3. Ejecuta el siguiente comando para confirmar los cambios:

```bash
git commit -m "Mensaje descriptivo del commit"
```

Si se quiere realizar un commit con un mensaje más detallado, se puede omitir la opción `-m` y simplemente ejecutar:

```bash
git commit
```

Esto abrirá el editor de texto configurado en Git para que puedas escribir un mensaje de commit más extenso.

El mensaje del commit debe ser claro y descriptivo, indicando qué cambios se realizaron y por qué. Esto es importante para el mantenimiento del proyecto y para que otros desarrolladores entiendan el propósito de los cambios.


## Otros usos utiles

### Modificar el último commit

Si deseas modificar el último commit (por ejemplo, para cambiar el mensaje o agregar archivos), puedes usar:

```bash
git commit --amend
```

Esto abrirá el editor de texto para que puedas cambiar el mensaje del commit. Si has añadido nuevos archivos al área de preparación, estos también se incluirán en el commit modificado.

> Importante: Usa `--amend` solo si el commit no ha sido compartido con otros (por ejemplo, no fue pusheado a un repositorio remoto), ya que esto reescribe el historial.
> Si el commit ya fue compartido, considera hacer un nuevo commit en su lugar o forzar el push con `git push --force-with-lease`, pero tené cuidado al hacerlo.

### Confirmar todos los cambios automáticamente

Para confirmar todos los cambios en el área de trabajo sin necesidad de usar `git add`, puedes usar:

```bash
git commit -a -m "Mensaje del commit"
```