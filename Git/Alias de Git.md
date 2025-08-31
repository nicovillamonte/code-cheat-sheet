# Alias de Git

Los alias de Git son atajos personalizados que puedes crear para comandos de Git que usas con frecuencia. Esto te permite ahorrar tiempo y esfuerzo al escribir comandos largos o complejos.

## Crear un alias

Para crear un alias, utiliza el siguiente comando:

```bash
git config --global alias.nombre_alias 'comando_git'
```

Por ejemplo, para crear un alias para `git status`, puedes usar:

```bash
git config --global alias.st status
```

Después de ejecutar este comando, podrás usar `git st` en lugar de `git status`.

## Ver los alias existentes

Para ver una lista de todos los alias que has creado, utiliza el siguiente comando:

```bash
git config --get-regexp alias
```

## Eliminar un alias

Para eliminar un alias, utiliza el siguiente comando:

```bash
git config --global --unset alias.nombre_alias
```

## Ejemplos de alias útiles

Aquí tienes algunos ejemplos de alias útiles que puedes crear:

```bash
git config --global alias.co checkout
git config --global alias.br branch
git config --global alias.ci commit
git config --global alias.df diff
git config --global alias.lg "log --oneline --graph --all"
git config --global alias.last "log -1 HEAD"
git config --global alias.unstage "reset HEAD --"
git config --global alias.hist "log --pretty=format:'%h %ad | %s%d [%an]' --graph --date=short"
git config --global alias.type "cat-file -t"
git config --global alias.dump "cat-file -p"
```

Estos alias pueden hacer que trabajar con Git sea más eficiente y agradable. Siéntete libre de personalizarlos según tus necesidades y preferencias.