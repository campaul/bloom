/**
 * This file tweaks various browser behaviors to make more sense for a drawing
 * application.
 *
 * 1) Forward mousemove and mouseup events to elements when you drag outside
 *    them. This includes if you drag outside the window.
 * 2) Prevent things from scrolling when the mouse is dragged outside the
 *    window by adding the "noscroll" class to the body.
 * 3) Prevent mouse ctrl + wheel or ctrl + +/- from zooming page so they can
 *    instead zoom the viewport.
 * 4) Close popovers on mousedown instead of mouseup
 * 5) Prevent re-opening popovers when you click on the trigger for an already
 *    open popup. This is needed because of #4
 */
(() => {
    let dragTarget = null;
    let ignorePopupOpen = false;


    document.addEventListener('mousedown', (e) => {
        if (e.button !== 0) {
            return;
        }

        const popoverTarget = document.getElementById(
            e.target.attributes.popovertarget?.value
        );

        if (popoverTarget?.matches(':popover-open')) {
            ignorePopupOpen = true;
        }

        document.querySelectorAll('[popover]').forEach(el => {
            if (!el.contains(e.target)) {
                el.hidePopover();
            }
        });

        document.body.classList.add('noscroll');

        // Don't select anything inside an SVG because we'll frequently end up
        // accidentally grabbing peer elements and firing unnecessary events.
        dragTarget = e.target.ownerSVGElement || e.target;
    }, { capture: true });


    document.addEventListener('mousemove', (e) => {
        if (dragTarget === null) {
            return;
        } else if (dragTarget !== e.target && !dragTarget.contains(e.target)) {
            dragTarget.dispatchEvent(new MouseEvent('mousemove', {
                clientX: e.pageX,
                clientY: e.pageY,
                bubbles: true,
            }));
        }
    }, { capture: true });


    document.addEventListener('mouseup', (e) => {
        if (e.button !== 0) {
            return;
        }

        document.body.classList.remove('noscroll');

        if (dragTarget === null) {
            return;
        } else if (dragTarget !== e.target && !dragTarget.contains(e.target)) {
            dragTarget.dispatchEvent(new MouseEvent('mouseup', {
                clientX: e.pageX,
                clientY: e.pageY,
                bubbles: true,
            }));
        }

        dragTarget = null;
    }, { capture: true });


    document.addEventListener('contextmenu', (e) => {
        if (dragTarget !== null) {
            e.preventDefault();
        }
    }, { capture: true });


    document.addEventListener('wheel', (e) => {
        if (e.ctrlKey) {
            e.preventDefault();
        }
    }, { capture: true, passive: false });


    document.addEventListener('keydown', (e) => {
        if (e.ctrlKey && (e.key == '+' || e.key == '-')) {
            e.preventDefault();
        }
    }, { capture: true });

    document.addEventListener('beforetoggle', (e) => {
        if (ignorePopupOpen && e.newState == "open") {
            ignorePopupOpen = false;
            e.preventDefault();
            e.stopPropagation();
        }
    }, { capture: true });
})();
