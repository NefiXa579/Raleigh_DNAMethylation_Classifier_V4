// Replace Shiny's default "No file selected" placeholder with app-specific copy,
// and let clicking that text box open the file browser too (not just "Browse...").
document.addEventListener('DOMContentLoaded', function () {
  var fileInput = document.getElementById('idat_files');
  if (!fileInput) return;
  var group = fileInput.closest('.input-group');
  if (!group) return;
  var textBox = group.querySelector('input[type="text"]');
  if (textBox) {
    textBox.placeholder = 'Select all IDAT files — Pairs are detected automatically';
    textBox.style.cursor = 'pointer';
    textBox.addEventListener('click', function () {
      fileInput.click();
    });
  }
});
