# Comando `git remote`

El comando `git remote` se utiliza para gestionar las conexiones a repositorios remotos en Git. Los repositorios remotos son versiones del proyecto que están alojadas en servidores externos, como GitHub, GitLab o Bitbucket, y permiten colaborar con otros desarrolladores.

## Uso Básico

### Listar repositorios remotos

El siguiente comando mostrará una lista de los repositorios remotos y sus URL asociadas:

```bash
git remote -v
```

### Gestionar repositorios remotos

#### Agregar

Para agregar un nuevo repositorio remoto, utiliza el siguiente comando:

```bash
git remote add <nombre> <url>
```

Donde `<nombre>` es un alias para el repositorio remoto (por ejemplo, `origin`) y `<url>` es la URL del repositorio remoto.

Un ejemplo común es agregar un repositorio remoto llamado `origin`:

```bash
git remote add origin <url_del_repositorio>
```

Y otro ejemplo para agregar un repositorio remoto llamado `upstream`:

```bash
git remote add upstream <url_del_repositorio>
```

El `upstream` se utiliza comúnmente para referirse al repositorio original desde el cual se ha bifurcado (forked) un proyecto.

#### Eliminar

Para eliminar un repositorio remoto, utiliza el siguiente comando:

```bash
git remote remove <nombre>
```

#### Renombrar

Para renombrar un repositorio remoto, utiliza el siguiente comando:

```bash
git remote rename <nombre_antiguo> <nombre_nuevo>
```

#### Otras operaciones útiles

Esto mostrará información detallada sobre el repositorio remoto especificado:

```bash
git remote show <nombre>
```

Esto actualizará la información de los repositorios remotos:

```bash
git remote update
```

Esto eliminará las referencias a ramas remotas que ya no existen en el repositorio remoto:

```bash
git remote prune <nombre>
```

Esto cambiará la URL de un repositorio remoto existente:

```bash
git remote set-url <nombre> <nueva_url>
```

