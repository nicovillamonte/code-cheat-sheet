# Configuración Inicial de Git

Para comenzar a usar Git, es necesario realizar una configuración inicial. Esto incluye establecer tu nombre de usuario y correo electrónico, que se utilizarán en los commits.

## Configuración del Usuario

Para configurar tu nombre de usuario y correo electrónico, utiliza los siguientes comandos:

```bash
git config --global user.name "Tu Nombre"
git config --global user.email "tu.email@ejemplo.com"
```

Estos comandos establecen la configuración global, lo que significa que se aplicará a todos los repositorios en tu máquina. Si deseas configurar un usuario diferente para un repositorio específico, omite la opción `--global` y ejecuta los comandos dentro del directorio del repositorio.

## Ver la Configuración Actual

Para verificar la configuración actual de Git, puedes usar el siguiente comando:

```bash
git config --list
```

Esto mostrará una lista de todas las configuraciones actuales, incluyendo el nombre de usuario y el correo electrónico.

Para ver una configuración específica, puedes usar:

```bash
git config user.name
```

o

```bash
git config user.email
```

## Configuración del Editor de Texto

Si deseas cambiar el editor de texto predeterminado que Git utiliza para los mensajes de commit, puedes configurarlo con el siguiente comando:

```bash
git config --global core.editor "nombre_del_editor"
```

Por ejemplo, para usar Visual Studio Code como editor, puedes ejecutar:

```bash
git config --global core.editor "code --wait"
```

## Configuración de Alias

Puedes crear alias para comandos de Git para hacerlos más cortos y fáciles de recordar. Por ejemplo, para crear un alias para `git status`, puedes usar:

```bash
git config --global alias.st status
```

Esto te permitirá usar `git st` en lugar de `git status`.
