# Basecode for CSS presentations

## HTML

```html
<p id="fg-label" style="color: white;">flex: 1</p>
<main class="container">
  <div class="box"></div>
  <div class="box"></div>
  <div class="box"></div>
</main>
```

## CSS

```cs
body {
  background-color: #05070B;
  margin: 0;
  display: flex;
  align-items: center;
  justify-content: center;
  height: 100vh;
  padding: 0 2rem;
}

* {
  box-sizing: border-box;
}

.container {
  min-width: 2rem;
  width: 30rem;
  border: 1px solid #636363;
  border-radius: 10px;
  padding: 1rem 0;

  display: flex;
  gap: 1rem;
  transition: all 1s ease;

  align-items: center;
  justify-content: center;

  /* resize: horizontal;
  overflow: hidden; */
}

.box {
  cursor: pointer;
  width: 5em;
  height: 5em;

  background: #7622B2;
  background: linear-gradient(to top right, rgba(118, 34, 178, 1) 10%, rgba(3, 189, 200, 1) 100%);
  background-repeat: no-repeat;
  background-size: cover;
  border: 2px solid #D2D6E1;
  border-radius: 8px;

  transition: all 1s ease-in;

  display: flex;
  align-items: center;
  justify-content: center;
  color: white;
}
```

## JavaScript (opcional)

Solo en caso de que se quiera hacer una demostracion rapida en la que cambien estilos con una tecla rapida:

```js
const boxes = document.querySelectorAll('.box');
const label = document.getElementById('fg-label');

let valor = 0; // estado inicial

function aplicarEstilo() {
  boxes.forEach(b => (b.style.flexGrow = String(valor))); // Cambiar estilo
  if (label) label.textContent = `flex-grow: ${valor}`; // Cambiar texto
}

aplicarEstilo();

window.addEventListener('keydown', (e) => {
  if (e.key.toLowerCase() === 'p') { // Tecla rapida
    valor = valor === 0 ? 1 : 0;
    aplicarEstilo();
  }
});
```