# Comando `git status`

El comando `git status` se utiliza para mostrar el estado actual del repositorio de Git. Proporciona información sobre los archivos que han sido modificados, los que están en el área de preparación (staging area) y los que aún no han sido rastreados.

## Uso Básico

1. Abre una terminal o línea de comandos en tu repositorio local.
2. Ejecuta el siguiente comando:

```bash
git status
```

## Información proporcionada

Al ejecutar `git status`, verás una salida que incluye:

- **Rama actual**: Muestra en qué rama te encontrás y si está adelantada o atrasada respecto a su contraparte remota.
- **Cambios no preparados (unstaged)**: Archivos modificados o eliminados que todavía no fueron añadidos al área de preparación.
- **Cambios preparados (staged)**: Archivos listos para ser confirmados, ya sea nuevos, modificados, eliminados o renombrados.
- **Archivos no rastreados (untracked)**: Archivos que Git no está siguiendo y que no han sido añadidos nunca.
- **Archivos ignorados**: Archivos que no son rastreados debido a reglas definidas en `.gitignore` (solo visibles con `git status --ignored`).
- **Estado de integración**: Si estás en medio de una operación como `merge`, `rebase` o `cherry-pick`, mostrará información sobre el progreso y posibles conflictos.
- **Submódulos modificados**: Si un submódulo cambió de commit o tiene modificaciones internas.