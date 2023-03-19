import { args } from './args';
import { IndexResponse, IndexQuery, Filterset } from 'scmindex-common';
import * as rm from 'typed-rest-client/RestClient';
import { IRequestQueryParams } from 'typed-rest-client/Interfaces';

const restc: rm.RestClient = new rm.RestClient('scheme-index-url', args.url);

export async function loadPossibleParams(): Promise<string[]> {
    const resp = await restc.get<string[]>(`rest/filterset/${args.filterset}/params`);
    return resp.result;
}

export async function loadPossibleReturns(): Promise<string[]> {
    const resp = await restc.get<string[]>(`rest/filterset/${args.filterset}/returns`);
    return resp.result;
}

export async function loadFiltersets(): Promise<Filterset[]> {
    const resp = await restc.get<Filterset[]>('rest/filterset');
    return resp.result;
}

export async function runIndexQuery(q: IndexQuery, offset: number): Promise<IndexResponse> {
    const queryParameters: IRequestQueryParams = {
        params: {
            rows: args.rows,
            start: offset,
            facet: 'false',
            'return': q.returns,
            param: q.params,
            query: q.query
        }
    };
    const resp = await restc.get<IndexResponse>(`rest/filterset/${q.filterset}/search`, { queryParameters });
    return resp.result;
}
