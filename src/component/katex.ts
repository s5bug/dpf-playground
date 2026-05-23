/** biome-ignore-all lint/style/noNonNullAssertion: usage of shadowRoot means we know more than TSC */

export default async () => {
  if (!customElements.get('dpf-katex')) {
    const promiseKatex = import('katex')
    const promiseCssSource = import('./katex.scss?inline')

    const promiseCss = new CSSStyleSheet().replace(
      (await promiseCssSource).default,
    )
    const katex = await promiseKatex
    const css = await promiseCss

    class KatexElement extends HTMLElement {
      static observedAttributes = ['data-src'] as const

      parent: HTMLSpanElement

      constructor() {
        super()

        this.attachShadow({ mode: 'open' })
        this.shadowRoot!.adoptedStyleSheets = [css]

        this.parent = document.createElement('span')
        this.shadowRoot!.appendChild(this.parent)
      }

      attributeChangedCallback(
        name: string,
        oldValue: string | undefined,
        newValue: string,
      ) {
        if (name === 'data-src' && newValue !== oldValue) {
          katex.render(newValue, this.parent, { throwOnError: false })
        }
      }
    }
    customElements.define('dpf-katex', KatexElement)
  }
}
