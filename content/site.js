"use strict";

document.documentElement.classList.add("has-js");

document.addEventListener("DOMContentLoaded", () => {
  const topLevelItems = document.querySelectorAll(
    "#text-table-of-contents > ul > li"
  );

  topLevelItems.forEach((item) => {
    const link = item.querySelector(":scope > a");
    const submenu = item.querySelector(":scope > ul");

    if (!link || !submenu) return;

    item.classList.add("toc-top-level");
    submenu.classList.add("toc-collapsible");

    const button = document.createElement("button");
    button.type = "button";
    button.className = "toc-toggle";
    button.setAttribute("aria-expanded", "false");
    button.setAttribute("aria-label", `Show subsections for ${link.textContent}`);

    const setExpanded = (expanded) => {
      item.classList.toggle("is-expanded", expanded);
      button.setAttribute("aria-expanded", String(expanded));
      button.setAttribute(
        "aria-label",
        `${expanded ? "Hide" : "Show"} subsections for ${link.textContent}`
      );
    };

    button.addEventListener("click", () => {
      setExpanded(button.getAttribute("aria-expanded") !== "true");
    });

    link.addEventListener("click", () => setExpanded(true));
    item.insertBefore(button, link);

    if (
      location.hash &&
      Array.from(item.querySelectorAll("a")).some(
        (candidate) => candidate.getAttribute("href") === location.hash
      )
    ) {
      setExpanded(true);
    }
  });
});
