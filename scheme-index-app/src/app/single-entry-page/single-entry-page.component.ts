import { Component } from '@angular/core';
import { Title } from '@angular/platform-browser';
import { ActivatedRoute, RouterModule } from '@angular/router';
import { Observable, mergeMap } from 'rxjs';
import { IndexService } from '../index.service';
import { SearchItem } from '../index.types';
import { RouterLink, SearchItemComponent } from '../search-item/search-item.component';
import { CommonModule } from '@angular/common';

@Component({
    imports: [
        CommonModule,
        RouterModule,
        SearchItemComponent
    ],
    selector: 'app-single-entry-page',
    templateUrl: './single-entry-page.component.html',
    styleUrls: ['./single-entry-page.component.scss']
})
export class SingleEntryPageComponent {

    public entry: Observable<SearchItem>;

    constructor(
        route: ActivatedRoute,
        svc: IndexService,
        title: Title
    ) {
        this.entry = route.paramMap.pipe(
            mergeMap(params => {
                const filterset = decodeURIComponent(params.get('filterset') || '');
                const lib = decodeURIComponent(params.get('lib') || '');
                const name = decodeURIComponent(params.get('name') || '');
                return svc.get(filterset, lib, name);
            }));
        route.paramMap.subscribe(params => {
            const name = params.get('name');
            const t = name? `${decodeURIComponent(name)} | Scheme Index` : 'Scheme Index';
            title.setTitle(t);
        });
    }

    seachItemRouterResolver(_: SearchItem, type: 'param' | 'return' | 'tag' | 'name' | 'lib', value: string): RouterLink | null {
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
