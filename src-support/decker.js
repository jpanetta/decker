require('reveal.js/lib/js/head.min.js');
Reveal = require('reveal.js/js/reveal');
require('./decker.scss');
require('./fonts/roboto.css');
require('./fonts/source-code-pro.css');

window.addEventListener('ready', function (event) {
  fixAutoplayWithStart();
  if (Reveal.isReady()) {
    makeVertical();
  } else {
    Reveal.addEventListener('ready', makeVertical);
  }
});

function fixAutoplayWithStart() {
  for(let vid of document.getElementsByTagName("video")){
    vid.addEventListener('play', (e) => {
        const timeRegex = /#t=(\d+)/;
        const matches = e.target.currentSrc.match(timeRegex);
        if(matches.length > 0) {
            e.target.currentTime = matches[1];
        }
    });
  }
}

function makeVertical() {
  const subsections = document.getElementsByClassName("sub");
  const subsection_bundles = [];
  for (let i = 0; i < subsections.length; i++) {
    const subsection = subsections[i];
    if (subsection.nodeName !== "SECTION") {
      continue;
    }
    const bundle = [subsection];
    while (
      i + 1 < subsections.length &&
      subsection.nextElementSibling === subsections[i + 1]
    ) {
      i += 1;
      bundle.push(subsections[i]);
    }
    subsection_bundles.push(bundle);
  }

  for (let bundle of subsection_bundles) {
    const supersection = document.createElement("section");
    supersection.classList.add("slide");
    supersection.classList.add("level1");
    const section = bundle[0].previousElementSibling;
    section.parentNode.insertBefore(supersection, section);
    supersection.appendChild(section);
    for (let subsection of bundle) {
      supersection.appendChild(subsection);
    }
  }
  Reveal.sync();
  Reveal.setState(Reveal.getState());
}