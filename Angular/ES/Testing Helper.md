# Testing Helper

El helper (o ayuda) de testing es una clase que nos otorgará herramientas para facilitar la escritura de pruebas unitarias de componentes en Angular 2+. Lo que permitirá extraer la lógica de las pruebas unitarias de los componentes y mantener los tests mucho más limpios y fáciles de desarrollar.

Si se quiere ver aplicado se puede analizar el [ejemplo de Signals en Angular v16](https://github.com/uqbar-project/eg-conversor-signals-angular) realizado para la materia [Algoritmos 3](https://algo3.uqbar-project.org/) de la [Universidad Nacional de San Martín (UNSAM)](https://www.unsam.edu.ar/).

## ¿Qué hacíamos antes?

Tomemos de ejemplo la funcionalidad de obtener los elementos del DOM de un componente, para lo que necesitabamos obtener de alguna manera el atributo `data-testid` del elemento para obtenerlo a mano de la siguiente manera suponiendo que el boton que queremos obtener tiene el atributo `data-testid="save-button"`:

```typescript
const boton = fixture.debugElement.nativeElement.querySelector(`[data-testid="save-button"]`)
```

Esto parece una buena manera de desarrollarlo, pero si tenemos que hacerlo en varios tests y con varios elementos diferentes, se vuelve tedioso y repetitivo. Además, si el atributo `data-testid` cambia, tendremos que cambiarlo en todos los componentes donde lo estemos usando.

La solución fácil a este problema es crear una función que nos devuelva el elemento que queremos.

```typescript
function getByTestId(testId: string) {
    const resultHtml = fixture.debugElement.nativeElement
    return resultHtml.querySelector(`[data-testid="${testId}"]`)
}
```

Entonces cada vez que queramos hacer un `querySelector` de un elemento con el atributo `data-testid` podemos hacerlo de la siguiente manera:

```typescript
const boton = getByTestId('save-button')
```

Mucho mejor, ¿No?

¡Sí! Pero todavía se puede mejorar más. Ya que con esto estamos encontrandonos con dos problemas que van a molestarnos a la hora de desarrollar. 

- Estamos repitiendo código en cada componente que queramos testear.
- Si se necesitan realizar más funcionalidades, como por ejemplo, obtener una lista de elementos por el test-id, deberemos crear otras funciones que realicen y nos devuelvan lo que necesitamos. Lo que va a generar que tengamos muchas funciones que se encarguen de hacer lo mismo en diferentes secciones del código.

## Solución

Para solucionar estos problemas, vamos a crear una clase que nos devuelva los elementos que necesitemos adaptandolo al componente el cual se va a testear de manera dinámica. Para esto, vamos a crear una clase llamada `TestingHelper` que, inicialmente, va a tener una estructura similar a la siguiente:

```typescript   
import { ComponentFixture } from '@angular/core/testing'

export class TestingHelper<T> {
  // Atributos adicionales

  private constructor(
    private fixture: ComponentFixture<T>,
    // Atributos del constructor adicionales
  ) {}

  static createHelper<T>(fixture: ComponentFixture<T>) {
    const testingHelper = new TestingHelper(fixture)

    // Configuracion inicial del helper si es necesaria

    return testingHelper
  }

  getByTestId(testId: string) {
    const resultHtml = this.fixture.debugElement.nativeElement
    return resultHtml.querySelector(`[data-testid="${testId}"]`)
  }

  getAllByTestId(testId: string) {
    const resultHtml = this.fixture.debugElement.nativeElement
    return resultHtml.querySelectorAll(`[data-testid="${testId}"]`)
  }

  // Métodos adicionales que necesitemos para los tests
}
```

Esto nos da una mejor abstracción y reutilización de código, además de facilitar el desarrollo de los tests en Angular.

Como ejemplo de uno que suelo utilizar yo en el testeo de mis proyectos de Angular puede ser el siguiente:

```typescript
import { ComponentFixture } from '@angular/core/testing'
import { NumberSymbol, getLocaleNumberSymbol } from '@angular/common'

export class TestingHelper<T> {
  decimalSymbol: string = '.'

  private constructor(
    private fixture: ComponentFixture<T>,
    public locale: string
  ) {}

  static createHelper<T>(fixture: ComponentFixture<T>, locale: string = 'en') {
    const testingHelper = new TestingHelper(fixture, locale)
    testingHelper.decimalSymbol = getLocaleNumberSymbol(
      locale,
      NumberSymbol.Decimal
    )
    return testingHelper
  }

  getByTestId(testId: string) {
    const resultHtml = this.fixture.debugElement.nativeElement
    return resultHtml.querySelector(`[data-testid="${testId}"]`)
  }

  getAllByTestId(testId: string) {
    const resultHtml = this.fixture.debugElement.nativeElement
    return resultHtml.querySelectorAll(`[data-testid="${testId}"]`)
  }

  localeToNumber(value: string): number {
    return +value.replace(this.decimalSymbol, '.')
  }

  localeToNumberByTestId(testId: string): number {
    const value = this.getByTestId(testId).textContent
    return this.localeToNumber(value)
  }
}
```

Como se ve, en este le agregue un par de métodos adicionales que me ayudan a testear los valores de los elementos del DOM que tienen un valor numérico. Ya que según el locale que se utilice, el valor puede tener un separador decimal diferente.

## ¿Cómo utilizar el Helper en los tests?

Para utilizar el helper en los tests, lo primero que tenemos que hacer es crear una instancia del mismo en el `beforeEach` de los tests, así como lo solemos realizar con el `TestBed` de la librería de testing de Angular. Para esto, vamos a crear un atributo en el describe del test llamado `testingHelper` de tipo `TestingHelper` (Vamos a utilizar el primer ejemplo dado anteriormente) y lo vamos a inicializar en el `beforeEach` de la siguiente manera:

```typescript
describe('Test de un Componente', () => {
  let component: Componente
  let fixture: ComponentFixture<Componente>
  let testingHelper: TestingHelper<Componente>

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [Componente, AppComponent]
    })
    fixture = TestBed.createComponent(Componente)
    component = fixture.componentInstance
    fixture.detectChanges()

    testingHelper = TestingHelper.createHelper(fixture)
  })
...
```

El componente puede ser cualquier componente de Angular. Ahora podemos utilizar este helper en cada uno de los tests:

```typescript
it('Al presionar el boton de submit se muestra el nombre ingresado en el input en el resultado', async () => {
    const inputNombre = testingHelper.getByTestId('nombre-input')
    const botonSubmit = testingHelper.getByTestId('submit-button')
    
    inputNombre.value = "Nicolas"
    botonSubmit.click()

    fixture.detectChanges()

    const nombreResult = testingHelper.getByTestId('resultado-nombre').textContent
    const nombresTabla = testingHelper.getAllByTestId('nombre-tabla')

    expect(nombreResult).toEqual("Nicolas")
    expect(nombresTabla.length).toEqual(1)
})
```

Este ejemplo es solo para mostrar como funciona el helper, pero puede no ser la mejor manera de testear un componente. Sin embargo, podemos ver que con el helper pudimos extraer la lógica de los tests y mantenerlos más limpios y fáciles de desarrollar. Obtuvimos varios elementos con el metodo `getByTestId` y pudimos obtener una lista de elementos con el método `getAllByTestId`.


## Conclusiones

Las cosas más importantes que nos da el realizar este tipo de helpers es:

- Cuando queramos añadir funcionalidades a los tests, solo tendremos que agregarlas en el helper y no en cada uno de los tests.
- Solamente debemos crear una instancia del helper en cada test para utilizarlo, lo que facilita la reutilización de código.
- El código de los tests se vuelve más limpio y fácil de desarrollar.
- Es compatible con cualquier proyecto de Angular 2+, por lo que se puede desarrollar una vez y facilitar el desarrollo en todos los proyectos que se realicen.
- Se puede utilizar en cualquier tipo de test, ya sean unitarios, de integración o de extremo a extremo.
- Es adaptable a las necesidades de cada uno por su facilidad de modificación y desarrollo.

Y así podemos seguir contando, pero quedo claro que esta es una herramienta muy útil dentro del testing. En este ejemplo se utilizó con el framework de Angular, pero claramente se puede realizar esto en cada uno de los frameworks y lenguajes que se quieran testear, con diferentes funcionalidades para cada uno.

### Datos del cheat sheet

\- Autor: Nicolás Villamonte <br>
\- Fecha: 21/08/2023 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
