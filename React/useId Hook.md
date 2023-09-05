# useId - Hook de React

El hook `useId` es un hook que nos permite generar un id único para un elemento de la interfaz de usuario. A simple vista no parece tener sentido, pero vamos a enseñar cómo utilizarlo y en qué casos es importante utilizarlo.

Vamos a utilizar `React` con `TypeScript` en este ejemplo.

1. [Sintaxis](#sintaxis)
2. [Utilización del Id](#utilizacion)
3. [Ejemplo](#ejemplo)
   1. [Problema](#problema)
   2. [Solución](#solucion)
4. [Más Información](#mas-info)

- [Datos del cheat sheet](#cheat-sheet-data)


<h2 id="sintaxis">Sintaxis</h2>

```ts
const id = useId();
```

Lo único que debemos realizar para utilizar el hook es llamarlo en el componente que queremos que tenga un id único. El hook se encargará de generar el id único y se deberá almacenar el mismo en una constante para luego utilizarlo.


<h2 id="utilizacion">Utilización del Id</h2>

El id generado por el hook se puede utilizar en cualquier elemento de la interfaz de usuario, pero es especialmente útil para los elementos que tienen un atributo `for` que apunta a un `id` de otro elemento. Por ejemplo:

```html
<label htmlFor={id}>Nombre</label>
<input id={id} type="text" />
```

En el ejemplo anterior, el `label` tiene un atributo `for` que apunta al `id` del `input`. Esto permite que al hacer click en el `label`, el `input` reciba el foco.

<h2 id="ejemplo">Ejemplo</h2>

Vamos a ver un ejemplo de cómo utilizar el hook `useId` para solucionar un problema muy común cuando se utilizan componentes de React. Imaginemos que tenemos un componente que se encarga de renderizar una sección de un formulario, que contiene un `label` y un `input` que aplican la propiedad HTML `for`. Si no utilizaramos el hook `useId`, el código podría verse de la siguiente manera:

```tsx
const Component: FC<Props> = () => {
  return <>
    <label htmlFor="input-name">Nombre</label>
    <input id="input-name" type="text" />
  </>;
}
```

<h3 id="problema">Problema</h3>

Todo parece funcionar de maravilla cuando utilizamos el componente una sola vez, pero ¿qué pasa si utilizamos el componente dos veces en el mismo formulario?

```tsx
function App() {

  return (
    <div>
      <Component /> 
      <Component /> 
    </div>
  );
}
```

Lo que podremos ver es que los IDs que hemos definido de manera manual van a colisionar entre sí, y el `label` no va a poder apuntar al `input` correcto. Esto se puede verificar presionando en el primer `label` y luego en el segundo `label`, analizando en cada caso en qué `input` recibe el foco cada uno.

El segundo `label` no apunta al `input` correcto, y esto se debe a que ambos `input` tienen el mismo `id`. Cuando se renderiza quedaría algo como lo siguiente:

```tsx
<div>
  <label htmlFor="input-name">Nombre</label>
  <input id="input-name" type="text" />
  <label htmlFor="input-name">Nombre</label>
  <input id="input-name" type="text" />
</div>
```

<h3 id="solucion">Solución</h3>

Para solucionar este problema, podemos utilizar el hook `useId` para generar un id único para cada `input`:

```tsx
const Component: FC<Props> = () => {
  const inputNameId = useId();

  return <div>
    <label htmlFor={inputNameId}>Nombre</label>
    <input id={inputNameId} type="text" />
  </div>;
}
```

De esta manera, cada vez que se renderice el componente, se generará un id único para el `input` y el `label` podrá apuntar al `input` correcto.

```tsx
<div>
  <label htmlFor=":r0:">Nombre</label>
  <input id=":r0:" type="text" />
  <label htmlFor=":r1:">Nombre</label>
  <input id=":r1:" type="text" />
</div>
```

Por lo que debemos tener en cuenta el uso del hook `useId` cuando utilizamos componentes que al renderizar más de una vez, puede llegar a generar una colisión de IDs en el código HTML de la página.

<h2 id="mas-info">Más Información</h2>

- [Documentación oficial de React](https://react.dev/reference/react/useId) para el Hook `useId`.
- Otros artículos
  - [A Complete Guide to useId() Hook in React 18](https://hetdesai03.medium.com/a-complete-guide-to-useid-hook-in-react-18-22119ecfd87f)


<br>

<h3 id="cheat-sheet-data">Datos del cheat sheet</h3>

\- Autor: Nicolás Villamonte <br>
\- Fecha: 04/09/2023 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>