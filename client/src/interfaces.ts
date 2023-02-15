export interface IndexQuery {
    query: string;
    params: string[];
    returns: string[];
}

export interface FuncSignatureReturn {
    kind: 'or' | 'values' | 'return';
    items: FuncSignatureReturn[];
    type: string;
}

export interface FuncSignatureParam {
    name: string;
    types: string[];
}

export interface FuncSignature {
    type: 'function';
    variants: {
        params: FuncSignatureParam[];
        return: FuncSignatureReturn;
    }[];
}

export interface SyntaxSignature {
    type: 'syntax';
    literals: string[];
    patterns: {
        pattern: string;
        type?: string;
    }[];
}

export interface ValueSignature {
    type: 'value';
    value: FuncSignatureReturn;
}

export interface PatternSignature {
    type: 'pattern';
    patterns: string[];
}

export interface AlistSignature {
    type: 'alist';
    car: FuncSignatureParam;
    cdr: FuncSignatureParam;
}

export interface ListSignature {
    type: 'list';
    element: FuncSignatureParam;
}

export interface VectorSignature {
    type: 'vector';
    element: FuncSignatureParam;
}

export type Signature = FuncSignature | SyntaxSignature | ValueSignature | PatternSignature | AlistSignature | ListSignature | VectorSignature;

export interface SearchItem {
    lib: string;
    name: string;
    signature: Signature;
    tags: string[];
    subsignatures: {
        name: string;
        signature: Signature;
    }[];
    description: string;
}

export interface IndexResponse {
    total: number;
    items: SearchItem[];
}
