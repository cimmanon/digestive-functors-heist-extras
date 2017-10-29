function dfList(el) {
	var listTemplate = el.querySelector('[class$="inputListTemplate"]');
	var indices = document.querySelector('[name="' + el.id + '.indices"]') || el.querySelector('[name$=".indices"]');
	// TODO: tune this to work if the indexes are out of order (eg. 3,5,2,8,6)
	var nextIndex = parseInt(indices.value.match(/[0-9]+$/, 'g'), 10) + 1 || 0;

	var matchMinusOne = new RegExp("\-1");
	var matchListTemplate = new RegExp("inputListTemplate");
	var matchNumber = new RegExp("[0-9]", "g");

	el.addEventListener("click", manageCodes, false);

	function manageCodes(ev) {
		if (ev.target.name == 'add') { // TODO: tune this to work with nested form lists
			var clone = listTemplate.cloneNode(true);
			clone.className = clone.className.replace(matchListTemplate, 'inputListItem');
			clone.id = clone.id.replace(matchMinusOne, nextIndex);
			clone.style.display = '';
			clone.disabled = false;

			var f = clone.querySelectorAll('input, select, textarea, button, label, fieldset, output');
			for (var i = 0, len = f.length; i < len; i++) {
				f[i].id = f[i].id.replace(matchMinusOne, nextIndex);
				//f[i].className = f[i].className.replace(matchListTemplate, 'inputListItem');
				f[i].style.display = '';

				// output and label elements can have a `for` attribute
				if (f[i].getAttribute('for')) {
					f[i].setAttribute('for', f[i].getAttribute('for').replace(matchMinusOne, nextIndex));
				}

				f[i].disabled = false;

				if (f[i].name) {
					f[i].name = f[i].name.replace(matchMinusOne, nextIndex);
				}

				// this allows us to set specific values to the index value,
				// handy for manipulating radio values that span a list of forms
				if (f[i].getAttribute('data-value') == 'index') {
					f[i].value = nextIndex;
				}
			}

			// if the add button and the template element have the same parent node,
			// insert the clone ahead of the add button.  otherwise, insert it as
			// last child of the template's parent element.
			if (listTemplate.parentNode == ev.target.parentNode) {
				ev.target.parentNode.insertBefore(clone, ev.target);
			} else {
				listTemplate.parentNode.appendChild(clone);
			}
			indices.value = (indices.value.length == 0 ? '' : indices.value + ',') + nextIndex;
			nextIndex++;
		} else if (ev.target.name && ev.target.name.match(/\.remove$/)) { // TODO: tune this to work with nested form lists
			// find the parent element for the given remove button's listItem
			var listItem = document.getElementById(ev.target.name.replace(/\.remove$/, ''));
			// remove it
			listItem.parentNode.removeChild(listItem);
			// adjust the indices to account for the removed element
			var oldCounter = listItem.id.match(matchNumber).pop();
			var regex = new RegExp("(^|,)" + oldCounter + "(,|$)");
			indices.value = indices.value.replace(regex, '');
		}
	}
}

var lists = document.querySelectorAll('.inputList');
for (var i = 0; i < lists.length; i++) {
	dfList(lists[i]);
}
