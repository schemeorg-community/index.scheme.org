import { Component } from '@angular/core';
import { ActivatedRoute, Params, Router } from '@angular/router';
import { Observable, mergeMap } from 'rxjs';
import { FiltersetsService } from '../filtersets-service.service';
import { SearchItem } from '../model';
import { RouterLink } from '../search-item/search-item.component';

@Component({
  selector: 'app-single-entry-page',
  templateUrl: './single-entry-page.component.html',
  styleUrls: ['./single-entry-page.component.scss']
})
export class SingleEntryPageComponent {

    public entry: Observable<SearchItem>;

    constructor(
        route: ActivatedRoute,
        svc: FiltersetsService
    ) {
        this.entry = route.paramMap.pipe(
            mergeMap(params => {
                return svc.get(params.get('filterset') || '', params.get('lib') || '', params.get('name') || '');
            }));
    }

    seachItemRouterResolver(item: SearchItem, type: 'param' | 'return' | 'tag' | 'name' | 'lib', value: string): RouterLink | null {
        switch (type) {
            case 'name':
                return null;
            case 'param':
                return {
                    routerLink: ['../../search'],
                    queryParams: { 'return': value }
                };
            case 'return':
                return {
                    routerLink: ['../../search'],
                    queryParams: { 'param': value }
                };
            case 'tag':
                return {
                    routerLink: ['../../search'],
                    queryParams: { 'tag': value }
                };
            case 'lib':
                return {
                    routerLink: ['../../search'],
                    queryParams: { 'lib': value }
                };
        }
    }
}
