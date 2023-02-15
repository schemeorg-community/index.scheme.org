import { ReplaySubject, Observable, map, combineLatest } from 'rxjs';
import { Component, ViewChild, ElementRef } from '@angular/core';
import { ActivatedRoute, Router, Params } from '@angular/router';
import { FiltersetsService } from '../filtersets-service.service';
import { IndexResponse, IndexQuery, SearchItem } from '../model';

@Component({
  selector: 'app-search-page',
  templateUrl: './search-page.component.html',
  styleUrls: ['./search-page.component.scss']
})
export class SearchPageComponent {

    @ViewChild('results_container')
    resultsContainer?: ElementRef;

    indexQuery: Observable<IndexQuery>;
    response = new ReplaySubject<IndexResponse>(1);
    filterpaneParams: Observable<FilterpaneParams>;
    pagerParams: Observable<PagerParams | null>;
    results: Observable<SearchItem[]>;
    facetCollapsed = false;

    constructor(
        private route: ActivatedRoute,
        private router: Router,
        private svc: FiltersetsService
    ) {
        this.indexQuery = combineLatest(route.paramMap, route.queryParams).pipe(map(([params, queryParams]) => {
            const filterset = params.get('filterset') || '';
            const query = queryParams['query'] || undefined;
            let page: number | undefined;
            if (queryParams['page']) {
                page = +(queryParams['page'] || 0);
            } else {
                page = undefined;
            }
            let libs: string[] | undefined = undefined;
            if (queryParams['lib'])
                libs = Array.isArray(queryParams['lib'])? queryParams['lib'] : [queryParams['lib']];
            let tags: string[] | undefined = undefined;
            if (queryParams['tag'])
                tags = Array.isArray(queryParams['tag'])? queryParams['tag'] : [queryParams['tag']];
            let p: string[] | undefined = undefined;
            if (queryParams['param'])
                p = Array.isArray(queryParams['param'])? queryParams['param'] : [queryParams['param']];
            let returns: string[] | undefined = undefined;
            if (queryParams['return'])
                returns = Array.isArray(queryParams['return'])? queryParams['return'] : [queryParams['return']];
            return {
                filterset,
                query,
                page,
                libs,
                tags,
                params: p,
                returns
            };
        }));
        this.indexQuery.subscribe(q => {
            svc.query(q).subscribe(resp => this.response.next(resp));
            if (this.resultsContainer)
                this.resultsContainer.nativeElement.scroll({ 
                    top: 0, 
                    left: 0, 
                    behavior: 'smooth' 
                });
        });
        this.filterpaneParams = combineLatest(this.indexQuery, this.response).pipe(map(([query, response]) => {
            return {
                query,
                response
            };
        }));
        this.pagerParams = combineLatest(this.indexQuery, this.response).pipe(map(([query, response]) => {
            if (!response.total)
                return null;
            return {
                pageSize: 40,
                total: response.total,
                page: query.page || 1
            };
        }));
        this.results = this.response.pipe(map(resp => resp.items));
    }

    onFacetCollapseChange(collapsed: boolean) {
        this.facetCollapsed = collapsed;
    }

    onPageClick(page: number) {
        this.router.navigate(
            [],
            {
                relativeTo: this.route,
                queryParams: { page },
                queryParamsHandling: 'merge'
            });
    }

    onQueryChange(query: IndexQuery) {
        this.router.navigate(
            [],
            {
                relativeTo: this.route,
                queryParams: {
                    query: query.query,
                    lib: query.libs,
                    param: query.params,
                    return: query.returns,
                    tag: query.tags,
                    page: query.page
                }
            });
    }

    seachItemRouterResolver(type: 'param' | 'return' | 'tag', value: string): { routerLink: string[], queryParams: Params } {
        switch (type) {
            case 'param':
                return {
                    routerLink: [],
                    queryParams: { 'return': value }
                };
            case 'return':
                return {
                    routerLink: [],
                    queryParams: { 'param': value }
                };
            case 'tag':
                return {
                    routerLink: [],
                    queryParams: { 'tag': value }
                };
        }
    }
}

interface FilterpaneParams {
    query: IndexQuery;
    response: IndexResponse;
}

interface PagerParams {
    total: number;
    page: number;
    pageSize: number;
}
