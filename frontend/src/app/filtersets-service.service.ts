import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { IndexResponse, IndexQuery } from './model';
import { Observable, shareReplay } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
//TODO rename
export class FiltersetsService {

  constructor(private http: HttpClient) {}

  public filtersets: Observable<string[]> = this.load().pipe(shareReplay());

  private load() {
      //TODO parameterize url
    return this.http.get<string[]>("/rest/filterset");
  }

  public query(request: IndexQuery) {
      let params = new HttpParams();
      if (request.query)
          params = params.set('query', request.query);
      if (request.page)
          params = params
            .set('start', (request.page - 1) * 40)
            .set('rows', 40);
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

}
