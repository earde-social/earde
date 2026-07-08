/* Thread shell view — page-scoped collapse for comment subtrees.
   Loaded only on /c/:slug/t/:id-:slug (via the shell's head_extra). The optimistic-vote
   handler and confirmModal live in the global layout script, so this file owns only collapse.
   Same id scheme (comment-content-<id> / comment-children-<id>) the server renders, mirroring
   the legacy post_page so behavior is identical. */
function toggleComment(id, btn) {
  const content = document.getElementById('comment-content-' + id);
  const children = document.getElementById('comment-children-' + id);
  if (!content) return;
  const isCollapsed = content.classList.contains('hidden');
  if (isCollapsed) {
    content.classList.remove('hidden');
    if (children) children.classList.remove('hidden');
    btn.innerText = '[-]';
  } else {
    content.classList.add('hidden');
    if (children) children.classList.add('hidden');
    btn.innerText = '[+]';
  }
}
