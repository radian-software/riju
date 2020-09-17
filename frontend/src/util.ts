const MAX_AUTOCOMPLETE_ITEMS = 5;

export function autocomplete(
  input: HTMLInputElement
): (results: string[]) => void {
  let results: string[] = [];
  let currentIndex = -1;
  let origValue = input.value;

  const close = () => {
    currentIndex = -1;
    for (const elt of Array.from(
      input.parentElement!.getElementsByClassName("autocomplete-items")
    )) {
      input.parentElement!.removeChild(elt);
    }
  };

  const updateActive = () => {
    Array.from(
      input.parentElement!.querySelectorAll(".autocomplete-items div")
    ).forEach((elt, index) => {
      if (index === currentIndex) {
        elt.classList.add("autocomplete-active");
      } else {
        elt.classList.remove("autocomplete-active");
      }
    });
    if (currentIndex === -1) {
      input.value = origValue;
    } else if (currentIndex >= 0 && currentIndex < results.length) {
      input.value = results[currentIndex];
    }
  };

  // input.addEventListener("blur", () => autocompleteClose(input));
  input.addEventListener("keydown", (e) => {
    if (currentIndex === -1) {
      origValue = input.value;
    }
    switch (e.key) {
      case "ArrowUp":
        currentIndex -= 1;
        if (currentIndex < -1) {
          currentIndex = results.length - 1;
        }
        updateActive();
        e.preventDefault();
        break;
      case "ArrowDown":
        currentIndex += 1;
        if (currentIndex >= results.length) {
          currentIndex = -1;
        }
        updateActive();
        e.preventDefault();
        break;
      case "Enter":
        close();
        break;
    }
  });
  return (newResults: string[]) => {
    results = newResults.slice(0, MAX_AUTOCOMPLETE_ITEMS);
    if (document.activeElement !== input) return;
    close();
    if (results.length === 0) return;
    const eltsDiv = document.createElement("div");
    eltsDiv.classList.add("autocomplete-items");
    results.forEach((result, index) => {
      const eltDiv = document.createElement("div");
      eltDiv.innerText = result;
      eltDiv.addEventListener("click", () => {
        currentIndex = index;
        updateActive();
        close();
      });
      eltsDiv.appendChild(eltDiv);
    });
    input.parentNode!.appendChild(eltsDiv);
  };
}
