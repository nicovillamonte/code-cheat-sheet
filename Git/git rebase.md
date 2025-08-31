# Comando `git rebase`

El comando `git rebase` se utiliza para integrar cambios de una rama a otra, aplicando los commits de una rama sobre otra. Es una alternativa al comando `git merge` y se utiliza comúnmente para mantener un historial de commits más limpio y lineal.

## Sintaxis

```bash
git rebase [opciones] [rama_base]
```

## Rebase normal vs. rebase interactivo

El rebase normal aplica los commits de la rama actual sobre la rama base especificada, mientras que el rebase interactivo permite modificar, reordenar, combinar o eliminar commits antes de aplicarlos. El rebase interactivo se inicia con la opción `-i`.


Cuando el diálogo de rebase interactivo se abre, tenés la capacidad de hacer varias cosas con los commits:

1. **Cambiar el orden de los commits**  
   Simplemente moviendo las líneas hacia arriba o hacia abajo en la lista.

2. **Ignorar un commit (eliminarlo)**  
   Cambiando la palabra `pick` al inicio de la línea por `drop` o `d`.

3. **Squashear commits (combinarlos)**  
   Cambiando la palabra `pick` de los commits que quieras combinar por:
   - `squash` o `s`: combina y mantiene los mensajes.  
   - `fixup` o `f`: combina pero descarta el mensaje del commit que se une.

4. **Reescribir el mensaje de un commit**  
   Usando `reword` o `r`. El contenido del commit queda igual, pero te pide un nuevo mensaje.

5. **Editar un commit**  
   Usando `edit` o `e`. Git se detiene en ese commit y te permite modificar su contenido (código, archivos) antes de continuar.

6. **Mantener un commit tal cual está**  
   Usando `pick` o `p` (la opción por defecto).


### Resumen de comandos:
- `pick` / `p` → mantener el commit.  
- `reword` / `r` → cambiar solo el mensaje.  
- `edit` / `e` → pausar para modificar el commit.  
- `squash` / `s` → combinar con el anterior, manteniendo mensajes.  
- `fixup` / `f` → combinar con el anterior, descartando el mensaje.  
- `drop` / `d` → eliminar el commit.


## Uso común

### Rebase de una rama sobre otra

Para aplicar los commits de la rama actual sobre otra rama (por ejemplo, `main`), utiliza:

```bash
git checkout nombre_de_rama
git rebase main
```

### Rebase interactivo

El rebase interactivo permite modificar, reordenar, combinar o eliminar commits antes de aplicarlos. Para iniciar un rebase interactivo, utiliza:

```bash
git rebase -i main
```

Esto abrirá un editor de texto donde podrás realizar las modificaciones deseadas en los commits.

### Continuar un rebase después de resolver conflictos

Si encuentras conflictos durante un rebase, resuélvelos manualmente y luego utiliza:

```bash
git rebase --continue
```

### Omitir un commit problemático

Si encuentras un commit que causa conflictos y deseas omitirlo, utiliza:

```bash
git rebase --skip
```

### Abortando un rebase

Si deseas cancelar el rebase y volver al estado anterior, utiliza:

```bash
git rebase --abort
```

### Pushear después de un rebase

Después de realizar un rebase, es posible que necesites forzar el push de la rama actual al repositorio remoto, ya que el historial de commits ha cambiado. Utiliza:

```bash
git push --force
```

o para mayor seguridad:

```bash
git push --force-with-lease
```

> Advertencia: Forzar un push puede sobrescribir los cambios en el repositorio remoto. Asegúrate de que nadie más esté trabajando en la misma rama antes de hacerlo.


### Rebase automático al hacer pull

Para configurar Git para que realice un rebase automáticamente al hacer `git pull`, utiliza:

```bash
git config --global pull.rebase true
```

Esto hará que `git pull` utilice rebase en lugar de merge por defecto. 