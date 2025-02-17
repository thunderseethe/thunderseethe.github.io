import * as wasm from "./onde.js";

function calculate_curves(start, end, num_curves) {
  let n = num_curves - 2;
  var curves = [end];
  let dt = 100 / n;
  for(let i = n - 1; i >= 0; i -= 1) {
    var t = 0.5 * dt + i * dt;
    var nt = 100 - t;
    curves.push(`color-mix(in srgb, ${start} ${nt}%, ${end} ${t}%)`);
  }
  curves.push(start);
  return curves;
}

const ondeSvg = document.querySelector("svg#onde");
function onde(config) {
  let width = config.width;
  let height = config.height;
  let step = config.width / config.steps;

  let arr = new Uint32Array(config.curve_colors.length);
  window.crypto.getRandomValues(arr);

  let colors = config.curve_colors;
  let y_incr = config.separation;

  let overall_height = y_incr * arr.length + height;
  ondeSvg.setAttributeNS(null, "viewBox", "0 0 " + width + " " + overall_height);
  ondeSvg.setAttributeNS(null, "height", overall_height + "px");

  var y = y_incr * arr.length;

  while(ondeSvg.lastChild) {
    ondeSvg.removeChild(ondeSvg.lastChild);
  }

  arr.forEach((seed, i) => {
    let curve = wasm.generate(
      seed,
      0.0,
      y,
      width,
      height,
      step,
    );
    let d = `M0 0 V${y} ${curve.replace(/M0 [\d]+ /, '')} V0 Z`;
    y -= y_incr;

    let newpath = document.createElementNS('http://www.w3.org/2000/svg',"path");
    newpath.setAttributeNS(null, "d", d);
    newpath.setAttributeNS(null, "stroke", colors[i]);
    newpath.setAttributeNS(null, "stroke-width", "0.1");
    newpath.setAttributeNS(null, "fill", colors[i]);
    ondeSvg.appendChild(newpath);
  })
}

async function generateNavSVG() { 
  let wasm_promise = wasm.default({ module_or_path: onde_wasm_path });

  let start = "var(--acc-color)";
  let end = "var(--last-wave-color)";
  let num_curves = 5;
  let curves = calculate_curves(start, end, num_curves);

  let width = document.documentElement.clientWidth;
  let config = {
    width: width,
    height: 15,
    separation: 15,
    steps: 20,
    curve_colors: curves,
  };

  await wasm_promise;

  onde(config);

  ondeSvg.setAttributeNS(null, "opacity", "1");
  ondeSvg.style.display = "block";

  let delay = 100;
  var timeout;
  window.addEventListener('resize', function() {
    clearTimeout(timeout);
    timeout = setTimeout((evt) => {
      let cfg = config;
      cfg.width = window.innerWidth;
      onde(cfg);
    }, delay);
  });
}

await generateNavSVG();
