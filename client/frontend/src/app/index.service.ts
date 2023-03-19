import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { IndexResponse, IndexQuery, Filterset, Download, SearchItem } from 'scmindex-common';
import { Observable, shareReplay, map, catchError, of } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class IndexService {

  constructor(private http: HttpClient) {}

  public filtersets: Observable<Filterset[]> = this.load().pipe(shareReplay());

  public downloads: Observable<Download[]> = this.loadDownloads().pipe(shareReplay());

  public filtersetNameMap: Observable<{[key: string]: string}> = this.filtersets.pipe(map(filtersets => {
      const m: {[key: string]: string}  = {};
      filtersets.forEach(f => m[f.code] = f.name);
      return m;
  }));

  private load() {
    return this.http.get<Filterset[]>("/rest/filterset");
  }

  private loadDownloads() {
    return this.http.get<Download[]>("/downloads.json").pipe(
        catchError(_ => of([]))
    );
  }

  public query(request: IndexQuery) {
      let params = new HttpParams();
      if (request.query)
          params = params.set('query', request.query);
      if (request.page)
          params = params
            .set('start', (request.page - 1) * 40)
            .set('rows', 40)
      if (request.libs)
          for (const l of request.libs)
              params = params.append('lib', l);
      if (request.params)
          for (const p of request.params)
              params = params.append('param', p);
      if (request.returns)
          for (const r of request.returns)
              params = params.append('return', r);
      if (request.tags)
          for (const t of request.tags)
              params = params.append('tag', t);
    return this.http.get<IndexResponse>("/rest/filterset/" + request.filterset + "/search", {
      params
    });
  }

  public get(filterset: string, lib: string, name: string): Observable<SearchItem> {
      return this.http.get<SearchItem>(`/rest/filterset/${filterset}/${lib}/${name}`);
  }

}
