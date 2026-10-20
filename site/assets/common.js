// Shared helpers for the status pages. status.json is written by
// scripts/build_status.R.

const CHECK_CLASS = { OK: "ok", NOTE: "note", WARNING: "warning", ERROR: "error", FAILURE: "error" };

// create an element: h("a", { href: "..." }, "text", child, ...)
function h(tag, attrs, ...children) {
  const node = document.createElement(tag);
  for (const [key, value] of Object.entries(attrs || {})) {
    if (value === null || value === undefined || value === false) continue;
    if (key === "class") node.className = value;
    else if (key.startsWith("on")) node.addEventListener(key.slice(2), value);
    else node.setAttribute(key, value);
  }
  for (const child of children.flat()) {
    if (child === null || child === undefined || child === false) continue;
    node.append(child instanceof Node ? child : document.createTextNode(String(child)));
  }
  return node;
}

function badge(text, cls, href, title) {
  return h(href ? "a" : "span", { class: "badge " + (cls || ""), href, title }, text);
}

function cranBadge(p) {
  const c = p.cran;
  if (c.status === "cran") return badge(c.version, "ok", c.url, "On CRAN, published " + (c.published || ""));
  if (c.status === "archived") return badge("archived", "warning", c.url, c.comment || "Archived on CRAN");
  return badge("not on CRAN", "");
}

function cranCheckBadge(p) {
  const chk = p.cran.checks;
  if (!chk || !chk.worst) return badge("–", "");
  return badge(chk.worst, CHECK_CLASS[chk.worst], chk.url, "Worst result over CRAN check flavours");
}

function universeBadge(p) {
  const u = p.runiverse;
  if (!u) return badge("not listed", "");
  if (u.build_status !== "success") return badge("build failed", "error", u.build_url);
  const worst = u.checks && u.checks.worst;
  return badge(worst || "built", CHECK_CLASS[worst] || "ok", u.url, "r-universe checks, " + u.universe);
}

function healthLabel(health) {
  return { ok: "OK", warning: "Needs attention", error: "Action needed" }[health] || health;
}

function relativeTime(iso) {
  if (!iso) return "";
  const days = Math.floor((Date.now() - new Date(iso).getTime()) / 86400000);
  if (days < 1) return "today";
  if (days < 2) return "yesterday";
  if (days < 60) return days + " days ago";
  if (days < 730) return Math.round(days / 30.4) + " months ago";
  return Math.round(days / 365.25) + " years ago";
}

function formatDate(iso) {
  if (!iso) return "";
  return new Date(iso).toLocaleDateString(undefined, { year: "numeric", month: "short", day: "numeric" });
}

async function loadStatus() {
  const res = await fetch("status.json", { cache: "no-cache" });
  if (!res.ok) throw new Error("could not load status.json (" + res.status + ")");
  const data = await res.json();
  const updated = document.getElementById("updated");
  if (updated) {
    updated.textContent = "Updated " + new Date(data.generated).toLocaleString(undefined, {
      dateStyle: "medium", timeStyle: "short"
    });
  }
  return data;
}

function showError(container, err) {
  container.replaceChildren(h("div", { class: "error-box" }, "The status data could not be loaded: " + err.message));
}
