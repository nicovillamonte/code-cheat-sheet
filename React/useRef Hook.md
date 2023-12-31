# useRef - Hook de React

El hook `useRef` nos permite hacer referencia a un valor mutable que se mantendrá entre renders. Es decir, que el valor de la referencia persistirá durante toda la vida del componente.

Vamos a utilizar `React` con `TypeScript` en este ejemplo.

1. [Sintaxis](#sintaxis)
2. [Casos de uso](#casos-de-uso)
   1. [Flag](#flag)
   2. [Guardar estados previos](#guardar-estados-previos)
   3. [Enlazar a elementos del DOM](#enlazar-a-elementos-del-dom)
   4. [Hacer focus en un elemento al cargar el componente](#hacer-focus-en-un-elemento-al-cargar-el-componente)
   5. [Otros usos...](#otros-usos)
3. [Más Información](#más-información)

- [Datos del Cheat Sheet](#datos-del-cheat-sheet)


## Sintaxis

```ts
const ref = useRef(initialValue);
```

La referencia se inicializa con el argumento pasado (`initialValue`). Por ejemplo podemos incializar la referencia con el valor numérico `0` y que por cada renderizado del componente se incremente en `1`, demostrando que no se vuelve a inicializar al volver a renderizar el componente volviendo a su valor inicial como lo haría una variable normal.

```ts
const Component = () => {
    const ref = useRef(0);

    ref.current += 1;
    console.log(ref.current)

    ...
}
```

La propiedad `.current` del objeto devuelto se inicializará con el argumento pasado (`initialValue`). Y luego podemos acceder al valor y modificarlo con esta misma propiedad.


## Casos de uso

Vamos a ver algunos ejemplos en los que se puede llegar a utilizar el useRef gracias a la persistencia de su valor entre renders de un mismo componente.

### Flag

Podemos utilizar el `useRef` para crear una flag que nos permita tener cualquier tipo de información, por ejemplo si ya se ha interactuado alguna vez con un elemento específico del componente.

```tsx
const Component = () => {
    const flag = useRef(false);

    const handleClick = () => {
        if(!flag.current)
            console.log("First click!");
        flag.current = true;
    }

    return (
        <button onClick={handleClick}>Click me!</button>
    )
}
```

### Guardar estados previos

Podemos utilizar el `useRef` para guardar estados previos de un componente. Por ejemplo, podemos guardar el estado anterior de un componente para poder compararlo con el estado actual y realizar alguna acción en base a eso.

```tsx
const Component = () => {
    const [state, setState] = useState(0);
    const prevState = useRef(0);

    const handleClick = () => {
        if(state == prevState.current)
            console.log("State doesn't change!");
        prevState.current = state;
    }

    return (
        <>
            <input type="number" value={state} onChange={e => setState(parseInt(e.target.value))} />
            <button onClick={handleClick}>Submit</button>
        </>
    )
}
```

En este codigo podemos ver que si el estado actual es igual al estado anterior que se haya seleccionado cuando se presiono el botón, se muestra un mensaje en la consola. Y luego de eso, se actualiza el estado anterior con el estado actual.



### Enlazar a elementos del DOM

Se ve muy seguido el uso del `useState` para manejar un formulario en React de manera **controlada**, lo que tiene sus ventajas y desventajas, siendo una de las últimas el constante renderizado del componente cada vez que se modifica el valor de un input, por ejemplo.

Si queremos evitar esto, podemos manejar el formulario de manera **no controlada** con el `useRef`.

```tsx
const Component = () => {
    const inputRef = useRef<HTMLInputElement>(null);

    const handleSubmit = (e: React.FormEvent<HTMLFormElement>) => {
        e.preventDefault();
        console.log(inputRef.current?.value);
    }

    return (
        <form onSubmit={handleSubmit}>
            <input type="text" ref={inputRef} />
            <button type="submit">Submit</button>
        </form>
    )
}
```

Con este código, recibimos el valor del input cuando se presiona el botón de submit, sin necesidad de estar actualizando un estado constantemente cada vez que se modifica el valor del input.

> **IMPORTANTE**: No es buena práctica el uso desmedido del `useRef` para manejar formularios. Recordar que siempre existe la manera vanilla de manejar formularios con JavaScript. Por lo tanto, hay que saber elegir en cada situación si utilizar un `useRef`, `useState` o la manera vanilla, teniendo en cuenta los conceptos de manejo de formularios de forma controlada y no controlada.


### Hacer focus en un elemento al cargar el componente

Podemos utilizar el `useRef` para hacer focus en un elemento específico cuando se carga el componente.

```tsx
const Component = () => {
    const inputRef = useRef<HTMLInputElement>(null);

    useEffect(() => {
        inputRef.current?.focus();
    }, [])

    return (
        <input type="text" ref={inputRef} />
    )
}
```

### Otros usos...

Es importante entender que aquí se están mostrando muchos ejemplos de uso del `useRef` de forma didáctica para entenderlo en su totalidad. Pero no es recomendable utilizarlo para todos estos casos, ya que existen otras maneras de hacerlo que son más simples y claras.


## Más Información

- [Documentación oficial de React](https://es.react.dev/reference/react/useRef) para el Hook `useRef`.
- Otros artículos
  - [¿Cómo rayos funciona el hook useRef en React?](https://dev.to/duxtech/como-rayos-funciona-el-hook-useref-en-react-2lah)
  - [React useRef() Hook Explained in 3 Steps](https://dmitripavlutin.com/react-useref/)
  

<br>

### Datos del Cheat Sheet

\- Autor: Nicolás Villamonte <br>
\- Fecha: 02/09/2023 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
\- Herramientas y Versiones: React 18.2.0, Nodejs 18.14.0