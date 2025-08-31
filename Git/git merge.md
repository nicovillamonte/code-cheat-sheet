# Comando `git merge`

El comando `git merge` se utiliza para combinar los cambios de dos ramas diferentes en un repositorio de Git. Este comando es fundamental para integrar el trabajo realizado en diferentes ramas y mantener un historial coherente.

Lo que se mezcla es la historia de los commits de la rama especificada en la rama actual. El merge va a crear un nuevo commit de fusión si hay cambios que integrar. Si no hay cambios, simplemente se actualizará el puntero de la rama actual. (caso de un **fast-forward** merge).


## Fast-Forward Merge y Recursive Merge

En un **fast-forward merge**, si la rama que se está fusionando es una continuación directa de la rama actual, Git simplemente mueve el puntero de la rama actual hacia adelante. No se crea un nuevo commit de fusión.

En un **recursive merge**, si hay cambios en ambas ramas, Git crea un nuevo commit de fusión que combina los cambios de ambas ramas. Este tipo de fusión es más común cuando las ramas han divergiendo y tienen commits diferentes.

## Sintaxis

```bash
git merge <rama>
```

## Ejemplo

1. Primero, asegúrate de estar en la rama donde deseas aplicar los cambios:

```bash
git checkout rama-principal
```

2. Luego, ejecuta el comando `git merge` seguido del nombre de la rama que deseas fusionar:

```bash
git merge rama-secundaria
```

## Resolución de Conflictos

Si hay conflictos entre las ramas, Git te pedirá que los resuelvas manualmente. Después de resolver los conflictos, debes agregar los cambios y completar la fusión:

```bash
git add .
git commit -m "Resolución de conflictos"
```

## Conclusión

El comando `git merge` es una herramienta poderosa para la colaboración en proyectos de desarrollo. Permite integrar cambios de diferentes ramas de manera eficiente y mantener un historial de cambios claro y coherente.