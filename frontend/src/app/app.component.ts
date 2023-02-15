import { Component, HostBinding } from '@angular/core';
import { NavigationEnd, Router  } from '@angular/router';
import { filter, map, Observable } from 'rxjs';
import { combineLatest } from 'rxjs';
import { FiltersetsService } from './filtersets-service.service';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.scss']
})
export class AppComponent {

  navbarItems: Observable<NavbarItem[]>;

  constructor(filtersetSvc: FiltersetsService, router: Router) {
      this.navbarItems = combineLatest(filtersetSvc.filtersets, router.events).pipe(
          filter(([_, event]) => event instanceof NavigationEnd),
          map(([filtersets, _]) => {
              return [{
                  label: 'Home',
                  link: '/',
                  isActive: router.isActive('/', true),
                  items: []
              }, {
                  label: 'Search',
                  isActive: router.isActive(router.parseUrl('/filterset'), false), //TODO
                  items: filtersets.map(f => { return {
                          label: f,
                          link: `filterset/${f}/search`,
                          isActive: false,
                          items: []
                      };
                  })
              }];
          }));

      this.selectedTheme = window.localStorage.getItem('theme') || 'default';
  }

  title = 'frontend';

  @HostBinding('class.theme-light')
  get lightTheme() {
      const prefersDark = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;
      return this.selectedTheme == 'light' || (this.selectedTheme == 'default' && !prefersDark);
  }

  @HostBinding('class.theme-dark')
  get darkTheme() {
      const prefersDark = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;
      return this.selectedTheme == 'dark' || (this.selectedTheme == 'default' && prefersDark);
  }

  selectedTheme: string;

  public onSelectTheme(theme: string) {
      window.localStorage.setItem('theme', theme);
  }

}

interface NavbarItem {
    label: string;
    link?: string;
    items: NavbarItem[];
    isActive: boolean;
}
