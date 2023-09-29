# Signals - Angular

Los signals en Angular, desde la versión 16, comienzan a mostrar el lado reactivo del framework. Los mismos permiten la creación de relaciones reactivas entre datos esto quiere decir que cuando un valor cambia, los valores que dependen de él también son notificados y se actualizan automáticamente.

Si se quiere se puede ver el ejemplo del [conversor de millas a kilómetros con Signals](https://github.com/uqbar-project/eg-conversor-signals-angular).

1. [¿Cómo funciona todo SIN signals?](#¿cómo-funciona-todo-sin-signals)
   1. [ZoneJS](#zonejs)
   2. [Problema](#problema)
2. [La solución: Signals](#la-solución-signals)
3. [Creación de un signal](#creación-de-un-signal)
4. [Consumir un signal con su getter](#consumir-un-signal-con-su-getter)
5. [Emitir un signal (Producers)](#emitir-un-signal-producers)
   1. [Set](#set)
   2. [Update](#update)
   3. [Mutate](#mutate)
6. [Consumir un signal (Consumers)](#consumir-un-signal-consumers)
   1. [Computed](#computed)
   2.  [Effect](#effect)
7. [Conclusión](#conclusión)
8. [Más información](#más-información)


- [Datos del Cheat Sheet](#datos-del-cheat-sheet)

> Al momento de escribirse este Cheat Sheet, Angular se encuentra en su versión 16. Por lo que debe estar ateneto a posibles cambios en futuras versiones.

## ¿Cómo funciona todo SIN signals?

### ZoneJS

Anteriormente a la version 16, siempre se utilizaba `zonejs` para detectar los cambios en los componentes.

Cuando se producía una cambio en la aplicación era detectado por `zonejs` y se activaba la deteccion de cambios en los componentes. En ese momento Angular comienza a recorrer todos los componentes del árbol de componentes y verifica si hay cambios en los estados de los mismos que afecten a la vista, si los hay, se actualizan en la vista.

### Problema

El problema que se presentaba con esta forma de deteccion de cambios es que se recorrian todos los componentes del árbol de componentes, sin importar si había cambios o no en los mismos. Esto generaba un consumo de recursos innecesario. Por lo que vinieron los **signals** a solucionar este problema.

## La solución: Signals

Los signals se basan en el concepto de `Producers` y `Consumers`. En el que los producers son los que emiten los cambios y los consumers son los que reciben los cambios sin la necesidad de recorrer todo el arbol en busca de donde se produjo el cambio. Por lo tanto, los signals permiten que los cambios se propaguen de forma eficiente.

## Creación de un signal

El proceso de crear un signal es muy simple. Solo debemos asignar a una variable el resultado de la función `signal` y pasarle como parametro el valor inicial del mismo.

``` typescript
millas = signal(0);
```

## Consumir un signal con su getter

La forma de obtener el valor de un signal tambien es muy sencilla, solamente hay que llamar a la constante en la que se almaceno el signal como si fuera un `getter`.

``` typescript
millas()
```

Por ejemplo, podemos utilizarlo en el HTML de la siguiente forma:

``` html
<div>millas()</div>
```

Desde aqui vamos a poder manejarlo de la misma manera que haciamos con otros parametros de los componentes. Por ejemplo aplicandoles pipes.

``` html
<div>{{millas() | number: '1.1-2':locale}}</div>
```

## Emitir un signal (Producers)

Para emitir un signal, debemos hacer un cambio en el valor del mismo. Para esto tenemos 3 opciones diferentes:

### Set

Para setear un valor _x_ en el signal, debemos llamar a la funcion `set` del mismo y pasarle como parametro el nuevo valor.

``` typescript
millas.set(53);
```

### Update

Para actualizar un valor _x_ en el signal, debemos llamar a la funcion `update` del mismo y pasarle como parametro una funcion que recibe como parametro el valor actual del signal y retorna el nuevo valor.

``` typescript
millas.update((millas) => millas + 1);
```

### Mutate

En el caso de que el valor del signal sea un objeto o un array, es decir, estados mutables, podemos utilizar la funcion `mutate` para modificar el valor interno del mismo.

``` typescript
conversiones = signal<Conversion[]>([]);

this.conversiones.mutate((conversiones) =>
  conversiones.push({ millas, kilometros })
);
```

## Consumir un signal (Consumers)

Ademas del `getter` que se utiliza en los signals para consumirlos. Existen otras dos formas diferentes de consumir un cambio emitido con alguno de los metodos anteriores.

### Computed

La funcion `computed` nos permite crear un signal que depende de otros signals. Es decir, que cuando alguno de los signals que dependen de él cambia, el signal `computed` también cambia.

``` typescript
kilometros = computed(() => millas() * 1.60934);
```

En este caso, cuando cambie el signal de millas, tambien va a cambiar el signal de kilometros utilizando el getter de millas y aplicandole la formula de conversion.

Podemos hacer cualquier tipo de operacion que nos devuelva un valor dentro de la funcion de computed. Lo mas importante de todo esto es que podemos utilizar un computed como condicional que solamente vamos a obtener una emición de su valor cuando esa condición cambie, por ejemplo:

``` typescript
valido = computed(() => millas() > 0);
```

En este caso estamos haciendo una validación de que el valor de millas sea mayor a 0. Si esto se cumple, el signal `valido` va a emitir un valor `true`, en caso contrario va a emitir un valor `false`. Pero mientras no se cumpla, aunque pasemos por los valores -5, -4, -3, -2 y -1, habiendo cambiado el valor 4 veces, el signal no va a emitir un cambio hasta que el valor de millas sea mayor a 0, recien ahi vamos a obtener un cambio en el signal `valido` y lo vamos a poder consumir. **Esto tiene un alto potencial.**

### Effect

La funcion `effect` se llama siempre en el constructor del componente y le podemos definir una función en su interior. Esa funcion se va a ejecutar cada vez que se produzca un cambio en alguno de los signals que se encuentren dentro de la misma.

``` typescript
effect(() => {
  console.log(millas());
});
```

En este caso cada vez que se cambien las millas se realizara un `console.log` con el nuevo valor.

Con esto podemos probar lo que decíamos del `computed` anteriormente. Vamos a hacer un computed signal que nos emita una señal cuando las millas cambien de decimal a entero y viceversa.

``` typescript
esDecimal = computed(() => Number(this.millas().toFixed(2)) % 1 !== 0);

constructor() {
  effect(() => {
    // Veremos como esto se llama solamente cuando cambia el valor de esDecimal
    console.log(this.esDecimal() ? 'Es Decimal' : 'Es Entero');
  });
}
```

Ese console.log se va a ejecutar solamente cuando cambie el valor de esDecimal, es decir, cuando las millas cambien de decimal a entero o viceversa.

<br>
Otro caso interesante es la comunicación entre componentes padre e hijo. Si obtenemos el valor del signal en el componente hijo y lo modificamos desde ahi, entonces se va a emitir el cambio en el signal y se va a actualizar el valor en el componente padre.

Componente hijo:
``` typescript
export class ComponenteHijo {
  @Input('conversiones') conversiones = signal<Conversion[]>([]);

  borrarConversion(index: number) {
    this.conversiones.mutate((lista) => lista.splice(index, 1));
  }
}
```

Componente padre:
``` typescript
export class ComponentePadre {
  // Lista de conversiones guardadas
  conversiones = signal<Conversion[]>([]);

  constructor() {
    effect(() => {
      // Este effect se va a ejecutar tanto desde los cambios de conversiones en este componente como en el componente hijo
      console.log(
        'Se hizo un cambio en lista conversiones',
        this.conversiones()
      );
    });
  }
}
```

HTML del componente padre:
``` html
<app-componente-hijo [conversiones]="conversiones"></app-componente-hijo>
```

En este caso, cuando se borre un item del signal pasado al componente hijo en el mismo, se va a emitir un cambio en el signal y se va a actualizar el valor en el componente padre. Por lo que se va a llamar al effect y podra ver en la consola del navegador. 


# Conclusión

A simple vista el `signal` no cambia nada en la interfaz grafica. Pero hace que la misma sea mucho mas eficiente en la deteccion de cambios, lo que se traduce en un mejor rendimiento de la aplicacion en comparacion con `ZoneJS`.

Anteriormente existía la posibilidad de hacer algo parecido con `BehaviorSubject` de `RxJS`, pero no era tan sencillo de implementar como lo es con los `signals`, ademas de que utiliza observables, lo que nos obliga a ser cuidadosos con las suscripciones y desuscripciones de los mismos.


## Más información

- [Documentación de Angular sobre signals](https://angular.io/guide/signals)
- [Core de los Signals en Angular](https://angular.io/api/core/signal)
- [Ejemplo de Signals en Angular con un conversor de millas a kilometros](https://github.com/uqbar-project/eg-conversor-signals-angular)

<br>

### Datos del Cheat Sheet

\- Autor: Nicolás Villamonte <br>
\- Fecha: 13/09/2023 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
\- Herramientas y Versiones: Angular V16