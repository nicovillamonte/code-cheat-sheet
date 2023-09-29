# useReducer - Hook de React

El hook `useReducer` de React nos permite manejar estados complejos de una forma más sencilla. El mismo se utiliza para manejar estados que tienen una lógica más compleja que un simple valor, como por ejemplo un array o un objeto que debe ser actualizado.

Vamos a utilizar `React` con `TypeScript` en este ejemplo.

1. [Sintaxis](#sintaxis)
2. [Creación de la función del reducer](#creación-de-la-función-del-reducer)
   1. [Ejemplo](#ejemplo)
3. [Creación del estado y dispatch con useReducer](#creación-del-estado-y-dispatch-con-usereducer)
4. [Casos de uso](#casos-de-uso)
5. [Más Información](#más-información)

- [Datos del Cheat Sheet](#datos-del-cheat-sheet)


## Sintaxis

```ts
const [state, dispatch] = useReducer(reducer, initialState);
```

El `useReducer` recibe como **primer argumento** el reducer, que es una función que recibe como argumentos el estado actual y la acción a ejecutar, y devuelve el nuevo estado.

Como **segundo argumento** recibe el estado inicial.

El `useReducer` devuelve un array con dos elementos:

- El **primer elemento** es el estado actual, que funciona de la misma manera que el estado de un componente creado con `useState`.
- El **segundo elemento** es el dispatch, que es una función que recibe como argumento la acción a ejecutar.

## Creación de la función del reducer

El reducer es una función que recibe como argumentos el estado actual y la acción a ejecutar, y devuelve el nuevo estado.

```ts
const reducer = (state: State, action: Action): State => {
    switch (action.type) {
        case "ACTION_1":
            return { ...state, ...action.payload };
        case "ACTION_2":
            return { ...state, ...action.payload };
        default:
            return state;
    }
};
```

Este es un ejemplo base sin ninguna funcionalidad. Pero vamos a realizar un ejemplo más completo para entenderlo mejor.

### Ejemplo

Vamos a desarrollar un contador simple que se pueda incrementar y decrementar en diferentes cantidades. Este es un ejemplo sencillo para entender el funcionamiento del `useReducer`. Pero en la práctica, es más recomendable utilizar el `useState` para este tipo de casos sencillos.

```ts
type CounterAction =
    | { type: "INCREMENT"; payload: number }
    | { type: "DECREMENT"; payload: number };

const counterReducer = (state: number, action: CounterAction): number => {
  switch (action.type) {
    case 'INCREMENT':
      return state + action.payload;
    case 'DECREMENT':
      return state - action.payload;
  }
};
```

El reducer recibe como argumentos el estado actual y la acción a ejecutar, y lo que devuelve es lo que se almacenará en el estado creado.

Las acciones suelen ser objetos con un tipo y un payload, el tipo refiere a la acción que se quiere realizar y el payload es información adicional que necesita para ejecutarla (opcional).

Vemos que definimos 2 tipos posibles de acciones, `INCREMENT` y `DECREMENT`, esto se puede realizar de manera más flexible, sin embargo hay que tener en cuenta que debe existir un caso _default_ en el switch:

```ts
type CounterAction = { type: string; payload: number }

const counterReducer = (state: number, action: CounterAction): number => {
  switch (action.type) {
    case 'INCREMENT':
      return state + action.payload;
    case 'DECREMENT':
      return state - action.payload;
    default:
     // Codigo a realizar si no se encuentra la accion
    //  Por ejemplo: return state;
  }
};
```

## Creación del estado y dispatch con useReducer

Una vez que tenemos el _Reducer_ creado podemos utilizarlo en la aplicación mediante el `useReducer`. Siguiendo con el ejemplo del contador:

```tsx
const [count, countDispatch] = useReducer(counterReducer, 0);
```

El estado inicial de `count` tendrá un valor de `0` y vamos a utilizar el `countDispatch` para ejecutar las acciones que definimos en el reducer.

Para ello, vamos a crear un componente en el que mostraremos el valor del estado y tendremos botones para incrementar y decrementar el contador en diferentes cantidades:

```tsx
const Counter = () => {
  const [count, countDispatch] = useReducer(counterReducer, 0);

  return (
    <div>
      <p>Count: {count}</p>

      <button onClick={() => countDispatch({ type: 'INCREMENT', payload: 1 })}>
        Increment
      </button>

      <button onClick={() => countDispatch({ type: 'DECREMENT', payload: 1 })}>
        Decrement
      </button>
    </div>
  );
};
```

Con este codigo podemos ver que el contador se incrementa y decrementa en 1 cada vez que se presiona el botón correspondiente.

Para ver que sí funciona el payload en nuestro reducer vamos a modificar el código para que el contador se incremente y decremente segun un valor de un input que vamos a agregar:

```tsx
const Counter = () => {
  const [count, countDispatch] = useReducer(counterReducer, 0);
  const [inputValue, setInputValue] = useState(1);

  return (
    <div>
      <p>Count: {count}</p>
      
      <button onClick={() => countDispatch({ type: 'INCREMENT', payload: inputValue })}>
        Increment
      </button>

      <button onClick={() => countDispatch({ type: 'DECREMENT', payload: inputValue })}>
        Decrement
      </button>

      <input
        type="number"
        placeholder="Steps"
        value={inputValue}
        onChange={(e) => setInputValue(parseInt(e.target.value))}
      />
    </div>
  );
};
```

## Casos de uso

La realidad es que el fin del hook `useReducer` es muy parecido al del `useState`. Entonces, ¿Cuándo debemos utilizar el `useReducer`?

- Cuando el estado tiene una lógica de estado compleja es recomendable utilizar el `useReducer`. Ésto permitirá limpiar nuestro componente de lógica compleja y mantenerlo más simple.
- Cuando el estado contiene múltiples subvaloresn y tienen varios formatos de actualización. Por ejemplo, si tenemos un array de objetos, podemos tener las operaciones de agregar, eliminar o actualizar un objeto en el arreglo. En estos casos, el `useState` puede volverse complejo de utilizar y el `useReducer` puede ser una mejor opción.


## Más Información

- [Documentación oficial de React](https://es.react.dev/reference/react/useReducer) del hook `useReducer`.
- [Cheat sheet del hook de useState](https://github.com/nicovillamonte/code-cheat-sheet/blob/main/React/useState%20Hook.md).
- Otros hooks pueden verse en este mismo repositorio. [Code Cheat Sheet - React](https://github.com/nicovillamonte/code-cheat-sheet/tree/main/React)
- Otros artículos sobre el tema:
  - [Understanding the Concept of React ( ) useReducer Hook](https://www.knowledgehut.com/blog/web-development/usereducer-in-react)
  - [React Hooks Tutorial – How to Use the useReducer Hook](https://www.freecodecamp.org/news/usereducer-hook-react/)


<br>

### Datos del Cheat Sheet

\- Autor: Nicolás Villamonte <br>
\- Fecha: 01/09/2023 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
\- Herramientas y Versiones: React 18.2.0, Nodejs 18.14.0