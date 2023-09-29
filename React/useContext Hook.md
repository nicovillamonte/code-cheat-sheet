# useContext - Hook de React

El [contexto de React](https://react-typescript-cheatsheet.netlify.app/docs/basic/getting-started/context/) se utiliza para gestionar datos globales y tiene la capacidad de proporcionar datos a los componentes sin importar qué tan profundos estén en el árbol de componentes. 

Vamos a utilizar `React` con `TypeScript` en este ejemplo.

1. [Creando el contexto](#creando-el-contexto)
2. [Proveyendo el contexto](#proveyendo-el-contexto)
3. [Consumiendo el contexto](#consumiendo-el-contexto)
4. [Crear un custom hook con contexto](#crear-un-custom-hook-con-contexto)
   1. [Creación del contexto y su provider](#creación-del-contexto-y-su-provider)
   2. [Creación del hook](#creación-del-hook)
   3. [Consumo del hook](#consumo-del-hook)
5. [Más Información](#más-información)
   
- [Datos del Cheat Sheet](#datos-del-cheat-sheet)


## Creando el contexto

Para crear un contexto, se utiliza la función `createContext` de React, que recibe como argumento el valor inicial del contexto.

```tsx
import { createContext } from 'react';

const Context = createContext<tipo>(defaultValue);
```

Este sería un ejemplo de un contexto custom que almacena si la aplicación esta en modo claro o modo oscuro:

```tsx
import { createContext } from 'react';

type Theme = "light" | "dark";

const ThemeContext = createContext<Theme>("light");
```

## Proveyendo el contexto

Para proveer el contexto a los componentes, se utiliza el componente `Context.Provider` que recibe como argumento el valor que se quiere proveer. Podemos envolver toda la aplciación o una parte de la misma.

```tsx
<Context.Provider value={value}>
    <MyComponent />
</Context.Provider>
```

En el siguiente ejemplo, se provee el contexto del modo claro u oscuro creado anteriormente a toda la aplicación:

```tsx
const root = ReactDOM.createRoot(document.getElementById('root') as HTMLElement)

root.render(
    <ThemeContext.Provider value="dark">
      <App />
    </ThemeContext.Provider>
)
```

## Consumiendo el contexto

Para consumir el contexto, se utiliza el hook `useContext` de React, que recibe como argumento el contexto que se quiere consumir y devuelve el valor que se provee en el `Context.Provider`.

```tsx
const value = useContext(Context);
```

En el siguiente ejemplo, se consume el contexto del modo claro u oscuro creado anteriormente:

```tsx
const theme = useContext(ThemeContext);
```

## Crear un custom hook con contexto

### Creación del contexto y su provider

Se puede crear un custom hook que utilice el contexto para proveer el valor del mismo a los componentes que lo consuman y reutilizar varios estados y funciones en diferentes componentes.

Para ello vamos a crear el contexto en un archivo `tsx` como el siguiente:

```tsx
export const Context = createContext();

export const ContextProvider = ({ children }) => {
    const [state, setState] = useState();

    return (
        <Context.Provider value={{ state, setState }}>
            {children}
        </Context.Provider>
    );
};
```

Vemos que agregamos a la fórmula un provider, el mismo tine la misma estructura que un componente en React. Dentro del mismo, definimos los estados y funciones que queremos utilizar en los componentes que consuman el contexto y los pasamos como valores del provider.

El `Context` es el que tenemos que consumir, mientras que el `ContextProvider` es el que nos va a proveer acceso al contexto.

### Creación del hook

Para crear el hook, en un archivo diferente del proyecto vamos a poder codificar algo como lo siguiente importando el contexto creado anteriormente y el hook `useContext` de React:

```tsx
export function useMyHook () {
  const { state, setState } = useContext(Context)

  const doSomething = () => {
    return "Used the custom context, state is: " 
            + state.toString()
  }

  return { state, doSomething, setState }
}
```

### Consumo del hook

Para consumir el hook, en el componente que queremos utilizarlo, importamos el hook creado anteriormente y lo utilizamos como cualquier otro hook:

```tsx
import { useMyHook } from '../hooks/myHook'

const MyComponent = () => {
  const { doSomething, setState } = useMyHook() // Podemos utilizar cualquier valor que nos provea el hook

  useEffect(() => {
    setState("New State")
  }, [])

  return (
    <div>
      <h1>{doSomething()}</h1>
    </div>
  )
}
```

## Más Información

- Se puede ver la [documentación de React del Hook useContext](https://react.dev/reference/react/useContext).
- Otros hooks pueden verse en este mismo repositorio. [Code Cheat Sheet - React](https://github.com/nicovillamonte/code-cheat-sheet/tree/main/React)
- Otros artículos sobre el tema:
  - [A Guide to React Context and useContext() Hook](https://dmitripavlutin.com/react-context-and-usecontext)
  - [Using useContext in React: a comprehensive guide](https://medium.com/@msgold/using-usecontext-in-react-a-comprehensive-guide-8a9f5271f7a8)

<br>

### Datos del Cheat Sheet

\- Autor: Nicolás Villamonte <br>
\- Fecha: 31/08/2023 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
\- Herramientas y Versiones: React 18.2.0, Nodejs 18.14.0