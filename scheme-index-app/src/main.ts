import { provideHttpClient } from '@angular/common/http';
import { isDevMode } from '@angular/core';
import { bootstrapApplication } from '@angular/platform-browser';
import { provideRouter } from '@angular/router';
import { AppComponent } from './app/app.component';
import { IndexPageComponent } from './app/index-page/index-page.component';
import { SearchPageComponent } from './app/search-page/search-page.component';
import { SingleEntryPageComponent } from './app/single-entry-page/single-entry-page.component';
import { provideServiceWorker } from '@angular/service-worker';

bootstrapApplication(AppComponent, {
    providers: [
    provideRouter([{
            path: '',
            component: IndexPageComponent,
            title: 'Scheme Index'
        }, {
            path: 'filterset/:filterset/search',
            component: SearchPageComponent,
            title: 'Scheme Index | Search'
        }, {
            path: 'filterset/:filterset/:lib/:name',
            component: SingleEntryPageComponent
        }]),
    provideHttpClient(),
    provideServiceWorker('ngsw-worker.js', {
        enabled: !isDevMode(),
        registrationStrategy: 'registerWhenStable:30000'
    }),
    provideServiceWorker('ngsw-worker.js', {
        enabled: !isDevMode(),
        registrationStrategy: 'registerWhenStable:30000'
    }),
    provideServiceWorker('ngsw-worker.js', {
        enabled: !isDevMode(),
        registrationStrategy: 'registerWhenStable:30000'
    })
]
});
