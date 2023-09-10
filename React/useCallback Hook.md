# useCallback - Hook de React

El hook `useCallback` nos permite crear una versión memorizada de una función, de manera que si la función se vuelve a crear en un nuevo renderizado, se devolverá la misma versión memorizada. 

> Tener muy en cuenta que no es común utilizar este hook, ya que la mayoría de las veces no es necesario.

1. [Sintaxis](#sintaxis)
2. [Ejemplo](#ejemplo)
   1. [¿Qué podemos hacer y cómo?](#que-podemos-hacer-y-como)
   2. [¿Qué pasa si no utilizamos useCallback?](#que-pasa-si-no-utilizamos-usecallback)
   3. [Agregamos el hook useCallback](#agregamos-el-hook-usecallback)
   4. [¿Vale la pena?](#vale-la-pena)
3. [Más Información](#mas-info)

- [Datos del cheat sheet](#cheat-sheet-data)

<h2 id="sintaxis">Sintaxis</h2>

```tsx
const cachedFn = useCallback(fn, dependencies);
```

Una manera más visual de entender cómo se utilizaría realmente es la siguiente:

```tsx
const functionName = useCallback(
  () => {
    doSomething(a, b);
  },
  [a, b],
);
```

El hook `useCallback` recibe dos argumentos:

- `fn`: La función que se quiere memorizar.
- `dependencies`: Un array de dependencias, valores reactivos, que solamente si éstos cambian, se volverá a crear la función.


<h2 id="ejemplo">Ejemplo</h2>

Pensemos por un momento en una aplicación en la que se deba ingresar un número en un input y se quiera mostrar el doble del mismo en un elemento `p`.

El ejemplo parece ser un poco confuso al principio, así que vamos a ir por partes.

> Este ejemplo es didáctico, ya que no va a ser necesario utilizar `useCallback` en este caso. Sin embargo, podemos entender cómo funciona el hook gracias a éste.

<h3 id="que-podemos-hacer-y-como">¿Que podemos hacer y cómo?</h3>

En primer lugar, vamos a enumerar las cosas que la aplicación nos debería permitir hacer:

- Ingresar un número en un input para ver cuál es su doble en un elemento debajo.
- Cambiar el color de fondo de la página con un botón.

Para estas dos funcionalidades vamos a utilizar el hook `useState` de React, que nos va a permitir crear estados reactivos. El primero nos permitirá tener un control sobre el valor del input, mientras que el segundo nos permitirá almacenar el estado actual del color de fondo de la página.

```tsx
const [inputValue, setInputValue] = useState(0);
const [background, setBackground] = useState("gray");
```

Para que el segundo funcione, de forma rápida, vamos a utilizar un `useEffect` que se ejecute cada vez que el valor del estado `background` cambie. En el mismo, vamos a cambiar el color de fondo de la página.

```tsx
useEffect(() => {
  document.body.style.backgroundColor = background;
}, [background])
```

Y, por último, la función que maneja el cambio de color cada vez que se presiona en el botón:

```tsx
const handleBackgroundChange = () => {
  background === "gray" ? setBackground("black") : setBackground("gray");
};
```

Con esto ya tenemos la funcionalidad del cambio de color de fondo de la página.

Aunque el cálculo que nos devolvería el valor del doble del número ingresado es muy sencillo y se puede realizar rápidamente inline, vamos a crear una función que se encargue del mismo para poder ver la diferencia entre utilizar el hook `useCallback` y no utilizarlo.

```tsx
const calculateDouble = () => {
  return inputValue * 2;
};
```

Ahora sí, con todo esto podemos maquetar la aplicación, pero lo vamos a hacer con una peculariedad. 

```tsx
return (
    <div>
        <input
        type="number"
        value={inputValue}
        onChange={(e) => setInputValue(Number(e.target.value))}
        />

        <Double calculate={calculateDouble}></Double>

        <button onClick={handleBackgroundChange}>Cambiar fondo</button>
    </div>
);
```

Vemos que utilizamos un componente que llamaremos `Double` que va a recibir como prop la función que calcula el doble del número ingresado. Este componente va a ser el encargado de mostrar el resultado del cálculo.

```tsx
type Props = {
  calculate: () => number;
};

const Double: FC<Props> = ({ calculate }) => {
  const [double, setDouble] = useState(0);

  useEffect(() => {
    console.log("Calculating double")
    setDouble(calculate());
  }, [calculate])

  return <p>{double}</p>;
};
```

Lo que hace este componente es, mediante un estado propio, almacenar el valor del doble del número ingresado utilizando la función llamada por parametro dentro de un `useEffect` que se ejecuta cada vez que el valor de la misma cambia.

<h3 id="que-pasa-si-no-utilizamos-usecallback">¿Qué pasa si no utilizamos <code>useCallback</code>?</h3>

Se puede notar que en el `useEffect` del componente `Double` se añadió un `console.log` para poder ver cuándo se ejecuta el mismo.

Lo que va a suceder en este caso es que cada vez que exista un re-renderizado del componente, se va a volver a crear la función `calculateDouble`, ya que la misma no se encuentra memorizada. Esto significa que, aunque el valor del estado `inputValue` no cambie, cuando el estado del color del fondo cambie, la función se va a volver a crear y el `useEffect` se va a volver a ejecutar.

------------------------------------------GIF DE LA APLICACIÓN SIN UTILIZAR `useCallback`

<h3 id="agregamos-el-hook-usecallback">Agregamos el hook <code>useCallback</code></h3>

Para solucionar este "problema" que podemos llegar a tener, vamos a utilizar el hook `useCallback` para memorizar la función `calculateDouble`.

```tsx
const calculateDouble = useCallback(() => {
  return inputValue * 2;
}, [inputValue]);
```

Con solo este cambio, la función `calculateDouble` se va a volver a crear solamente cuando el valor del estado `inputValue` cambie, ya que es la única dependencia que le pasamos al hook. Mientras tanto, aunque haya re-renderizado del componente por factores externos como el estado del color de fondo, la función no se va a volver a crear y el `useEffect` no se va a volver a ejecutar.

------------------------------------------GIF DE LA APLICACIÓN UTILIZANDO `useCallback`

> Aunque los hooks `useMemo` y `useCallback` son muy parecidos en lo que hacen, la diferencia entre ellos es que `useMemo` memoriza el valor de retorno de una función, mientras que `useCallback` memoriza la función en sí misma.

<h3 id="vale-la-pena">¿Vale la pena?</h3>

Esto es algo que no nos cambia mucho el rendimiento de la aplicación, al menos que la función que se este creando nuevamente en cada re-renderizado del componente sea muy costosa, lo que puede llegar a generar un problema de performance. Este sería el único caso en el que se debe utilizar el hook `useCallback`.

Por lo tanto, la respuesta es no, en la mayoría de los casos el costo de las funciones que se vuelven a crear en cada re-renderizado del componente no suele ser tan alto como para que sea necesario utilizar el hook `useCallback`.

<h2 id="mas-info">Más Información</h2>

- [Documentación de React](https://react.dev/reference/react/useCallback) del Hook useCallback.
- Cheat Sheet del hook [useMemo](https://github.com/nicovillamonte/code-cheat-sheet/blob/main/React/useMemo%20Hook.md)

<br>

<h3 id="cheat-sheet-data">Datos del cheat sheet</h3>

\- Autor: Nicolás Villamonte <br>
\- Fecha: 10/09/2023 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>



























