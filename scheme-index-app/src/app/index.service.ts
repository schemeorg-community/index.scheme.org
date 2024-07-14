import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { IndexResponse, IndexQuery, Filterset, SearchItem, SearchItemSingle, FuncSignatureReturn, ResponseFacetValue } from './index.types';
import { Observable, shareReplay, map, catchError, of, combineLatest } from 'rxjs';
import MiniSearch from 'node_modules/minisearch/dist/es';

@Injectable({
  providedIn: 'root'
})
export class IndexService {


  public types: Observable<SearchItem[]> = this.loadData().pipe(shareReplay());
  public filtersets: Observable<Filterset[]> = this.loadFilters().pipe(shareReplay());

  private filtersetFilter$: Observable<{[index: string]: Set}>;
  private searcher$: Observable<{ searcher: MiniSearch<SearchItemIndexingWrap>, all: SearchItemIndexingWrap[]}>;

  constructor(private http: HttpClient) {
      this.filtersetFilter$ = this.filtersets.pipe(map(filters => {
          const result: {[index: string]: Set} = {};
          for (let f of filters) {
              const set: Set = {};
              for (let lib of f.libs) {
                  set[lib] = true;
              }
              result[f.code] = set;
          }
          return result;
      }));
      this.searcher$ = this.types.pipe(map(data => this.buildSearcher(data)));
  }

  public filtersetNameMap: Observable<{[key: string]: string}> = this.filtersets.pipe(map(filtersets => {
      const m: {[key: string]: string}  = {};
      filtersets.forEach(f => m[f.code] = f.name);
      return m;
  }));

  private loadFilters() {
    return this.http.get<Filterset[]>("assets/filters.json");
  }

  private loadData() {
    return this.http.get<SearchItem[]>("assets/types.json");
  }

  public query(request: IndexQuery) {
      return combineLatest([this.searcher$, this.filtersetFilter$])
        .pipe(map(([searcher, filters]) => {
            return this.executeSearch(searcher.searcher, searcher.all, request, filters[request.filterset]);
        }));
  }

  public get(filterset: string, lib: string, name: string): Observable<SearchItem> {
      return combineLatest([this.searcher$, this.filtersetFilter$])
        .pipe(map(([searcher, filters]) => {
            for (let e of searcher.all) {
                if (!filters[filterset][e.data.lib])
                    continue;
                if (e.data.lib != lib) {
                    continue;
                }
                if (!e.names[name]) {
                    continue;
                }
                return e.data;
            }
            throw new Error('Not found');
        }));
  }

  private executeSearch(searcher: MiniSearch<SearchItemIndexingWrap>, all: SearchItemIndexingWrap[], request: IndexQuery, libs: Set): IndexResponse {
      function recordFacetValue(facet: {[index: string]: number }, value: string) {
          if (value == '#f')
              return;
          if (!facet[value]) {
              facet[value] = 0;
          }
          facet[value]++;
      }
      function recordFacetValues(facet: {[index: string]: number }, values: Set) {
          for (let v in values) {
              recordFacetValue(facet, v);
          }
      }
      function finalizeFacet(facet: {[index: string]: number}) {
          const facets: ResponseFacetValue[] = [];
          for (let k in facet) {
              facets.push({
                  value: k,
                  count: facet[k]
              });
          }
          facets.sort((a, b) => a.count - b.count);
          return facets;
      }
      const tagsFacets: { [index: string]: number } = {};
      const paramsFacets: { [index: string]: number } = {};
      const returnsFacets: { [index: string]: number } = {};
      const libsFacets: { [index: string]: number } = {};
      const config = {
          boost: {
              'name': 1000
          }
      };
      const found = request.query 
          ? searcher.search(request.query, config).map(r => all[r.id])
          : all;
      const start = 40 * ((request.page || 1) - 1);
      const end = start + 40;
      const returnData = [];
      let count = 0;
      top:
      for (let e of found) {
          if (!libs[e.data.lib]) {
              continue;
          }
          if (request.libs && request.libs.length) {
              let found = false;
              for (let l of request.libs) {
                  if (e.data.lib == l) {
                      found = true;
                      continue;
                  }
              }
              if (!found) {
                  continue top;
              }
          }
          if (request.returns) {
              for (let r of request.returns) {
                  if (!e.returns[r]) {
                      continue top;
                  }
              }
          }
          if (request.params) {
              for (let p of request.params) {
                  if (!e.params[p]) {
                      continue top;
                  }
              }
          }
          if (request.tags) {
              for (let t of request.tags) {
                  if (!e.tags[t]) {
                      continue top;
                  }
              }
          }
          if (count >= start && count < end) {
              returnData.push(e.data);
          }
          count++;
          recordFacetValue(libsFacets, e.data.lib);
          recordFacetValues(paramsFacets, e.params);
          recordFacetValues(returnsFacets, e.returns);
          recordFacetValues(tagsFacets, e.tags);
      }
      const result = {
          total: count,
          items: returnData,
          libs: finalizeFacet(libsFacets),
          params: finalizeFacet(paramsFacets),
          returns: finalizeFacet(returnsFacets),
          tags: finalizeFacet(tagsFacets)
      };
      return result;
  }

  private buildSearcher(data: SearchItem[]): { searcher: MiniSearch<SearchItemIndexingWrap>, all: SearchItemIndexingWrap[] } {
      const wrappedData = data.map((d, i) => this.wrapSearchItem(i, d));
      const searcher =  new MiniSearch<SearchItemIndexingWrap>({
          idField: 'id',
          fields: ['name', 'description']
      });
      searcher.addAll(wrappedData);
      return {
          searcher: searcher,
          all: wrappedData
      };
  }

  private wrapSearchItem(index: number, e: SearchItem): SearchItemIndexingWrap {
      const tags: Set = {};
      const params: Set = {};
      const returns: Set = {};
      const name: string[] = [];
      const names: Set = {};

      function processReturn(r: FuncSignatureReturn) {
          if (r.kind == 'return' && r.type != '...' && r.type != '*' && r.type != 'undefined') {
              returns[r.type] = true;
          } else {
              for (let r2 of (r.items || [])) {
                  processReturn(r2);
              }
          }
      }

      function process(e: SearchItemSingle) {
          names[e.name] = true;
          name.push(e.name);
          for (let tag of e.tags) {
              tags[tag] = true;
          }

          if (e.signature.type == 'function') {
              for (let variant of e.signature.variants) {
                  for (let param of variant.params) {
                      for (let type of param.types) {
                          params[type] = true;
                      }
                  }
                  processReturn(variant.return);
              }
          }

          if (e.signature.type == 'value') {
              processReturn(e.signature.value);
          }

          if (e.signature.type == 'syntax') {
              for (let pat of e.signature.patterns) {
                  if (pat.type) {
                      processReturn(pat.type);
                  }
              }
          }
      }

      if (e.kind == 'single') {
          process(e);
      } else if (e.kind == 'group') {
          for (let entry of e.entries) {
              process(entry);
          }
      }

      return {
          id: index,
          data: e,
          description: e.description,
          names: names,
          name: name.join(' '),
          tags,
          params,
          returns
      };
  }

}

type Set = { [index: string]: boolean; };

interface SearchItemIndexingWrap {
    id: number;
    data: SearchItem;
    name: string;
    description: string;
    names: Set;
    tags: Set;
    params: Set;
    returns: Set;
}

interface Searcher {
    data: SearchItem[];
}
