# useImperativeHandle - Hook de React

El hook `useImperativeHandle` nos permite exoner una referencia customizada de un componente hijo a su componente padre. Esto nos permite utilizar, de manera personalizada, las referencias de los componentes hijos en los componentes padres.

Para poder entender este hook es recomendable ver el Cheat Sheet del hook `useRef`, ya que se utilizará en conjunto con este hook.

Vamos a utilizar `React` con `TypeScript` en este ejemplo.

1. [Sintaxis](#sintaxis)
2. [UseRef y su problema en React](#utilizacion)
3. [Solución](#solucion)
   1. [forwardRef](#solucion-forwardref)
   2. [useImperativeHandle](#solucion-useimperativehandle)
4. [Ejemplo sencillo para entenderlo mejor](#ejemplo)
5. [¿Cuándo utilizar este Hook?](#cuando-utilizar-este-hook)
6. [Más Información](#mas-info)

- [Datos del cheat sheet](#cheat-sheet-data)


<h2 id="sintaxis">Sintaxis</h2>

```ts
useImperativeHandle(ref, createHandle, [deps]);
```

El hook `useImperativeHandle` recibe tres parámetros:

- `ref`: es la referencia que se va a exponer al componente padre.
- `createHandle`: es una función que devuelve un objeto con las propiedades y métodos que se van a exponer al componente padre.
- `deps`: es un array de dependencias que se encarga de la actualización de la referencia cuando cambia alguno de sus valores. Este parámetro es opcional, si no se pasa, la referencia se va a actualizar en cada renderizado del componente.


<h2 id="utilizacion">UseRef y su problema en React</h2>

El hook `useRef` nos permite crear una referencia mutable que persiste durante todo el ciclo de vida de un componente. Esto nos permite utilizar la referencia en cualquier parte del componente. Por ejemplo, podríamos realizar algo como lo siguiente para mostrarlo de ejemplo:

```tsx
function App() {
  const [value, setValue] = useState<string>('');
  const ref = useRef<HTMLInputElement>(null);

  return (
    <>
      <input
        type='text'
        ref={ref}
        value={value}
        onChange={(e) => setValue(e.target.value)}
      />
      <button onClick={() => ref.current?.focus()}>Focus</button>
    </>
  );
}
```

En este ejemplo, utilizamos el hook `useRef` para crear una referencia mutable que apunta al `input` del formulario. Luego, utilizamos la referencia para poder hacer foco en el `input` cuando hacemos click en el botón.

El problema surge si la referencia que queremos actualizar en este componente se encontraran dentro de un componente hijo. Por ejemplo:

```tsx
const MyInput = ({ ref }: { ref: React.RefObject<HTMLInputElement>}) => {
  return <input ref={ref} />;
};

const App = () => {
  const myInputRef = useRef<HTMLInputElement>(null);

  return (
    <>
      <MyInput ref={myInputRef} />
      <button onClick={() => myInputRef.current?.focus()}>Focus</button>
    </>
  );
};
```

Éste último ejemplo no va a funcionar ya que la referencia que se le pasa al componente hijo no es la misma que se utiliza en el componente padre. Esto se debe a que la referencia que se le pasa al componente hijo es una nueva referencia que se crea cada vez que se renderiza el componente padre. Por eso es que debemos exponer la referencia desde el componente hijo para que el padre la utilice.

Por lo tanto, cuando nos encontramos en una instancia de árbol de componentes y se necesita utilizar una referencia de un componente hijo en un componente padre, no nos basta con el hook `useRef`. E aquí es donde entra en juego el hook `useImperativeHandle`.

<h2 id="solucion">Solución</h2>

Vamos a solucionar el problema con el que nos topamos anteriormente utilizando el hook `useImperativeHandle` y explicaremos a detalle qué es lo que está sucediendo.

<h3 id="solucion-forwardref">forwardRef</h3>

Para esta solución, el primer paso es envolver el componente hijo con la función `forwardRef`. Esta función nos permite exponer la referencia que se le pasa al componente hijo desde el componente padre. Para ello, luego de las `props`, el componente hijo debe recibir la referencia que se le pasa desde el componente padre.

Siguiendo con el ejemplo:

```tsx
const MyInput = forwardRef((props, ref) => {
  const inputRef = useRef<HTMLInputElement>(null);

  return <input ref={inputRef} />;
});
```

Cuando las `props` no se van a utilizar como en este caso, se puede utilizar el símbolo `_` para indicar que no se van a utilizar.

```tsx
const MyInput = forwardRef((_, ref) => {
  ...
```

Vemos que de esta manera todavía no funciona, pues falta exponer la referencia que se le pasa al componente hijo desde el componente padre. Para ello, debemos utilizar el hook `useImperativeHandle`.

<h3 id="solucion-useimperativehandle">useImperativeHandle</h3>

Vamos a exponer la referencia en nuestro componente hijo envuelto en la función `forwardRef` utilizando el hook `useImperativeHandle`.

```tsx
const MyInput = forwardRef((_, ref) => {
  const inputRef = useRef<HTMLInputElement>(null);

  useImperativeHandle(ref, () => ({
    focus: () => {
      inputRef.current?.focus();
    },
  }), []);

  return <input ref={inputRef} />;
});
```

Lo que estamos haciendo en este caso es que la referencia que se pase al componente hijo pase a tener la función `focus()` que realiza el foco utilizando la referencia interna del componente hijo. Entonces en nuestro componente padre podremos hacer algo como lo siguiente:

```tsx
const App = () => {
  const myInputRef = useRef<HTMLInputElement>(null);

  return (
    <>
      <MyInput ref={myInputRef} />
      <button onClick={() => myInputRef.current?.focus()}>Focus</button>
    </>
  );
};
```

De esta manera, la referencia `myInputRef` va a tener en su valor actual los elementos del objeto que se creo en el hook `useImperativeHandle` del componente hijo. Por lo tanto, al hacer `myInputRef.current?.focus()` se va a ejecutar la función `focus()` que se definió en el hook `useImperativeHandle` del componente hijo. Que, a su vez, internamente ejecuta el `focus()` de la referencia interna del Input en el componente hijo.


<h2 id="ejemplo">Ejemplo sencillo para entenderlo mejor</h2>

Dentro de los elementos del objeto definido en el hook `useImperativeHandle`, se puede devolver cualquier cosa que se necesite. Vamos a hacer un ejemplo más sencillo con un `console.log` que, aunque no tenga mucho sentido, va a ayudar a entenderlo mejor.

Vamos a agregar otro botón en nuestro ejemplo que lo único que haga es imprimir en consola el valor del input mediante una función que vamos a llamar `printTheValue()`.

Anteriormente no obtuvimos ningún error con el `focus()` porque era un método definido dentro del objeto `HTMLInputElement`, sin embargo, la función `printTheValue()` no existe en el objeto `HTMLInputElement` ni en ningún objeto `HTMLElement`, por lo tanto, nos va a dar un error si agregamos directamente esta funcionalidad.

Para que esto no ocurra, vamos a cambiar el tipo de la referencia que se le pasa al componente hijo. Lo que nos obliga a crear un tipo de dato:

```tsx
type MyInputHandle = {
  focus: () => void;
  printTheValue: () => void;  // Función a agregar
}
```

Luego, vamos a cambiar el tipo de la referencia que se le pasa al componente hijo y agregar el nuevo botón:

```tsx
const App = () => {
  const myInputRef = useRef<MyInputHandle>(null); // Acá cambia el tipo de la referencia

  return (
    <>
      <MyInput ref={myInputRef} />
      <button onClick={() => myInputRef.current?.focus()}>Focus</button>
      <button onClick={() => myInputRef.current?.printTheValue()}>Show Value</button>
    </>
  );
};
```

Una vez que tenemos esto configurado vamos a agregar la funcionalidad en el componente hijo, además de mostrar cómo se le determina un tipo al `forwardRef`:

```tsx
const MyInput = forwardRef<MyInputHandle>((_, ref) => {
  const inputRef = useRef<HTMLInputElement>(null);

  useImperativeHandle(ref, () => ({
    focus: () => {
      inputRef.current?.focus();
    },
    printTheValue: () => {
      console.log(inputRef.current?.value);
    }
  }), []);

  return <input ref={inputRef} />;
});
```

Estar atenetos a que la referencia interna del componente hijo es de tipo `HTMLInputElement` porque refiere al input de HTML que tiene en su interior. Aquí se nota la diferencia entre esta referencia y la del componente padre, el cual es de tipo `MyInputHandle`, la cuál compatibiliza con el objeto que estamos devolviendo en el hook `useImperativeHandle`.

Ahora, cuando se haga click en el botón `Show Value`, se va a imprimir en consola el valor del input.

<h2 id="cuando-utilizar-este-hook">¿Cuándo utilizar este Hook?</h2>

En la documentación oficial de React, nos dicen que no debemos abusar del hook `useRef`, por lo tanto, tampoco debemos abusar del `useImperativeHandle`, siempre que se pueda se deben utilizar los estados para solucionar el problema que se necesite. Por lo tanto, se recomienda utilizarlos solo cuando sea realmente necesario.

<h2 id="mas-info">Más Información</h2>

- [Documentación oficial de React](https://react.dev/reference/react/useImperativeHandle) para el Hook `useImperativeHandle`.
- Otros artículos
  - [Ultimate Guide for useImperativeHandle Hook](https://medium.com/@ahsan-ali-mansoor/ultimate-guide-for-useimperativehandle-hook-6fbc955d6ea0)


<br>

<h3 id="cheat-sheet-data">Datos del cheat sheet</h3>

\- Autor: Nicolás Villamonte <br>
\- Fecha: 06/09/2023 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
