export const downloadJsonImpl = (filename) => (content) => () => {
  const blob = new Blob([content], { type: "application/json" });
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  a.download = filename;
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
  URL.revokeObjectURL(url);
};

export const pickJsonFileImpl = (onSuccess) => (onCancel) => () => {
  const input = document.createElement("input");
  input.type = "file";
  input.accept = ".json,application/json";
  input.onchange = () => {
    const file = input.files[0];
    if (!file) { onCancel(); return; }
    const reader = new FileReader();
    reader.onload = () => onSuccess(reader.result)();
    reader.onerror = () => onCancel();
    reader.readAsText(file);
  };
  input.click();
};
