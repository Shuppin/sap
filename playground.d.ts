/* tslint:disable */
/* eslint-disable */
/**
*/
export function do_some_stuff(): void;
/**
* @param {boolean} display_env
* @param {string} source
*/
export function interpret(display_env: boolean, source: string): void;
/**
* @param {string} source
*/
export function parse(source: string): void;
/**
* @param {string} source
*/
export function lex(source: string): void;

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
  readonly memory: WebAssembly.Memory;
  readonly do_some_stuff: () => void;
  readonly interpret: (a: number, b: number, c: number) => void;
  readonly parse: (a: number, b: number) => void;
  readonly lex: (a: number, b: number) => void;
  readonly __wbindgen_malloc: (a: number, b: number) => number;
  readonly __wbindgen_realloc: (a: number, b: number, c: number, d: number) => number;
}

export type SyncInitInput = BufferSource | WebAssembly.Module;
/**
* Instantiates the given `module`, which can either be bytes or
* a precompiled `WebAssembly.Module`.
*
* @param {SyncInitInput} module
*
* @returns {InitOutput}
*/
export function initSync(module: SyncInitInput): InitOutput;

/**
* If `module_or_path` is {RequestInfo} or {URL}, makes a request and
* for everything else, calls `WebAssembly.instantiate` directly.
*
* @param {InitInput | Promise<InitInput>} module_or_path
*
* @returns {Promise<InitOutput>}
*/
export default function __wbg_init (module_or_path?: InitInput | Promise<InitInput>): Promise<InitOutput>;
