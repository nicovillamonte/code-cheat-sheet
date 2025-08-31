# Conflictos en Git

Los conflictos en Git ocurren cuando dos ramas intentan modificar la misma línea de un archivo o cuando una rama elimina un archivo que otra rama ha modificado. Git no puede resolver estos conflictos automáticamente y requiere intervención manual.

## Resolución de Conflictos

Cuando se produce un conflicto, Git marcará los archivos en conflicto y te pedirá que los resuelvas. Para resolver un conflicto, sigue estos pasos:

1. Abre el archivo en conflicto en tu editor de texto.
2. Busca las marcas de conflicto (`<<<<<<<`, `=======`, `>>>>>>>`) que indican las diferencias entre las ramas.
3. Edita el archivo para resolver el conflicto, eliminando las marcas y eligiendo qué cambios conservar.
4. Guarda el archivo.
5. Agrega el archivo resuelto al área de preparación:

```bash
git add nombre_del_archivo
```

6. Completa la fusión con un commit:

```bash
git commit -m "Resolución de conflictos"
```

## Comandos Útiles para Manejar Conflictos

- **Ver el estado de los archivos en conflicto**:

```bash
git status
```

- **Ver las diferencias entre las versiones en conflicto**:

```bash
git diff
```

- **Cancelar la fusión y volver al estado anterior**:

```bash
git merge --abort
```