import { args } from './args';

import {
    FuncSignatureParam,
    FuncSignatureReturn,
    FuncSignature,
    SyntaxSignature,
    ValueSignature,
    PatternSignature,
    AlistSignature,
    ListSignature,
    VectorSignature,
    Signature,
    SearchItem,
    SpecValues,
    IndexResponse,
    IndexQuery
} from './interfaces';

import {
    loadPossibleParams,
    loadPossibleReturns,
    loadFiltersets,
    runIndexQuery
} from './repo';

import * as readline from 'readline';

import { EOL } from 'os';

const rl = readline.createInterface({
    input: process.stdin, 
    output: process.stdout,
});

function rlPrompt(p: string): Promise<string> {
    return new Promise((acc, rej) => {
        rl.question(p, (resp) => {
            acc(resp);
            rl.pause();
        });
    });
}

async function run() {
    if (args.query && args.filterset) {
        runSingleQuery();
    } else {
        runRepl();
    }
}

async function runSingleQuery() {
    const query = parseQuery(args.query);
    if (args.strict) {
        query.query = `name_precise: "${query.query}"`;
    }
    const resp = await runIndexQuery(query, 0);
    renderResp(0, resp);
    rl.close();
}

async function runRepl() {
    rl.write(`Using server: ${args.url}${EOL}`);
    rl.write(`Run with "--help" for help${EOL}`);
    if (!args.filterset) {
        args.filterset = await selectFilterset();
    }
    let lastQuery = null;
    let page = 0;
    while (true) {
        const queryString = (await rlPrompt(args.filterset + '> ')).trim();
        if (!queryString) {
            continue;
        }
        if (queryString == 'p:') {
            const params = await loadPossibleParams();
            params.forEach(p => {
                rl.write(p + EOL);
            });
            continue;
        }
        if (queryString == 'r:') {
            const returns = await loadPossibleReturns();
            returns.forEach(r => {
                rl.write(r + EOL);
            });
            continue;
        }
        let query;
        if (queryString.match(/^[0-9]+$/g)) {
            page = parseInt(queryString) - 1;
            query = lastQuery;
        } else {
            page = 0;
            query = parseQuery(queryString);
        }
        const resp = await runIndexQuery(query, page * args.rows);
        renderResp(page, resp);
        lastQuery = query;
    }
}

function renderResp(page: number, resp: IndexResponse) {
    const horSep = '----------------------------------------------------' + EOL;
    resp.items.forEach(item => {
        rl.write(horSep);
        rl.write(`${item.name}: ${item.signature.type}${EOL}`);
        if (item.signature.type == 'function') 
            renderFunction(item.signature);
        if (item.signature.type == 'syntax')
            renderSyntax(item.signature);
        if (item.signature.type == 'value')
            renderValue(item.signature);
        item.subsignatures
            .filter((s): s is { name: string; signature: PatternSignature } => s.signature.type == 'pattern')
            .forEach(s => renderPattern(s.name, s.signature));
        item.subsignatures
            .filter(s => s.signature.type != 'pattern')
            .forEach(s => {
                rl.write(`${s.name} => `);
                if (s.signature.type == 'function') 
                    renderFunction(s.signature);
                if (s.signature.type == 'value')
                    renderValue(s.signature);
                if (s.signature.type == 'list')
                    renderList(s.signature);
                if (s.signature.type == 'alist')
                    renderAList(s.signature);
                if (s.signature.type == 'vector')
                    renderVector(s.signature);
            });
        rl.write(EOL);
        if (item.tags.length) {
            const tags = item.tags.map(t => `[${t}]`).join(' ');
            rl.write(tags + EOL);
        }
        renderSpecValues(item.spec_values);
        rl.write(`From ${item.lib}${EOL}`);
    });
    rl.write(horSep);
    rl.write(`Page ${page + 1} / ${Math.ceil(resp.total / args.rows)}${EOL}`);
}

function paramToString(p: FuncSignatureParam): string {
    if (p.types.length) {
        return `[${p.types.join('/')} ${p.name}]`;
    } else {
        return p.name;
    }
}

function returnToString(r: FuncSignatureReturn): string {
    if (r.kind == 'return') {
        return r.type;
    } else {
        return `(${r.kind} ${r.items.map(returnToString).join(' ')})`;
    }
}

function renderFunction(s: FuncSignature) {
    s.variants.forEach(v => {
        const params = v.params.map(paramToString).join(' ');
        const ret = returnToString(v['return']);
        const str = `(lambda ${params}) => ${ret}`;
        rl.write(str + EOL);
    });
}

function renderSyntax(s: SyntaxSignature) {
    if (s.literals.length)
        rl.write(`Literals: ${s.literals.join(', ')}${EOL}`);
    s.patterns.forEach(p => {
        rl.write(p.pattern);
        if (p.type) {
            rl.write(` => ${p.type}`);
        }
        rl.write(EOL);
    });
}

function renderValue(s: ValueSignature) {
    rl.write(returnToString(s.value) + EOL);
}

function renderSpecValues(vals: SpecValues[]) {
    vals.forEach(v => {
        rl.write(`${v.field}:${EOL}`);
        v.values.forEach(e => {
            rl.write(`    ${e.value}: ${e.desc}${EOL}`);
        });
    });
}

function renderAList(lst: AlistSignature) {
    return `'((${paramToString(lst.car)} . ${paramToString(lst.cdr)}) ...)`;
}

function renderList(lst: ListSignature) {
    return `'(${paramToString(lst.element)} ...)`;
}

function renderVector(vec: VectorSignature) {
    return `#(${paramToString(vec.element)} ...)`;
}

function renderPattern(name: string, p: PatternSignature) {
    rl.write(`<${name}>:${EOL}`);
    p.patterns.forEach(pat => {
        rl.write(`    ${pat}${EOL}`);
    });
}

async function selectFilterset(): Promise<string> {
    const opts = await loadFiltersets();
    rl.write('Select filterset' + EOL);
    opts.forEach((f, i) => {
        rl.write(`${i + 1}. ${f}${EOL}`);
    });
    while (true) {
        const filtersetIndex = await rlPrompt('> ');
        const i = parseInt(filtersetIndex);
        if (i >= 1 && i <= opts.length) {
            return opts[i - 1];
        }
    }
}

function parseQuery(query: string): IndexQuery {
    const queryWords: string[] = [];
    const params: string[] = [];
    const returns: string[] = [];
    const words = query.split(/\s+/g);
    words.forEach(w => {
        if (w.startsWith('p:'))
            params.push(w.substring(2));
        else if (w.startsWith('r:'))
            returns.push(w.substring(2));
        else
            queryWords.push(w);
    });
    return {
        query: queryWords.join(' '),
        params,
        returns
    };
}

run();
