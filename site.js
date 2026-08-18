"use strict";

document.documentElement.classList.add("has-js");

document.addEventListener("DOMContentLoaded", () => {
  const topLevelItems = document.querySelectorAll(
    "#text-table-of-contents > ul > li"
  );

  topLevelItems.forEach((item, index) => {
    const link = item.querySelector(":scope > a");
    const submenu = item.querySelector(":scope > ul");

    if (!link) return;

    item.classList.add("toc-top-level");
    link.classList.add("toc-section-link");

    if (!submenu) return;

    submenu.classList.add("toc-collapsible");
    submenu.id = `toc-submenu-${index}`;
    link.setAttribute("aria-controls", submenu.id);
    link.setAttribute("aria-expanded", "false");

    const setExpanded = (expanded) => {
      item.classList.toggle("is-expanded", expanded);
      link.setAttribute("aria-expanded", String(expanded));
    };

    link.addEventListener("click", () => {
      setExpanded(link.getAttribute("aria-expanded") !== "true");
    });

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
