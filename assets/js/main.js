(() => {
  const modal = document.querySelector("[data-modal]");
  if (!modal) return;

  const modalImg = modal.querySelector("[data-modal-img]");
  const modalTitle = modal.querySelector("[data-modal-title]");
  const modalDownload = modal.querySelector("[data-modal-download]");
  const closeButtons = modal.querySelectorAll("[data-close]");

  const openModal = ({ src, title, downloadHref }) => {
    modal.setAttribute("aria-hidden", "false");
    document.documentElement.style.overflow = "hidden";

    if (modalImg) modalImg.src = src;
    if (modalImg) modalImg.alt = title || "Visualização ampliada";
    if (modalTitle) modalTitle.textContent = title || "Visualização";

    if (modalDownload) {
      modalDownload.href = downloadHref || "#";
      modalDownload.style.pointerEvents = downloadHref ? "auto" : "none";
      modalDownload.style.opacity = downloadHref ? "1" : "0.5";
    }
  };

  const closeModal = () => {
    modal.setAttribute("aria-hidden", "true");
    document.documentElement.style.overflow = "";
    if (modalImg) modalImg.removeAttribute("src");
  };

  document.addEventListener("click", (event) => {
    const button = event.target.closest(".media-btn");
    if (!button) return;

    const fullSrc = button.getAttribute("data-full");
    const title = button.getAttribute("data-title") || "";
    const download = button.getAttribute("data-download") || "";
    if (!fullSrc) return;

    openModal({ src: fullSrc, title, downloadHref: download });
  });

  closeButtons.forEach((el) => el.addEventListener("click", closeModal));

  document.addEventListener("keydown", (event) => {
    if (event.key === "Escape" && modal.getAttribute("aria-hidden") === "false") {
      closeModal();
    }
  });
})();

