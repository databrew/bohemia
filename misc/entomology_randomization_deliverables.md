# Entomology randomization deliverables

(Currently under development at


This document briefly describes the deliverables which Databrew will generate for the randomization component of the Bohemia project's entomology study.

## Upstream dependencies

Prior to _any_ entomology randomization deliverables being executed, the following steps need to occur sequentially:

1. The (already created) clusters in Mozambique need to each be assigned to one of three treatment arms: (i) human, (ii) human+livestock, (iii) control. This should only be done when the project gives a clear go-ahead and provides any instructions / rules / oversight conditions.

2. Clusters need to be created in Tanzania. This cannot be done until census data has finished in the not-yet-censed areas of Tanzania.

3. Tanzanian clusters need to be assigned to one of three treatment arms: (i) human, (ii) human+livestock, (iii) control. This should only be done when the project gives a clear go-ahead and provides any instructions / rules / oversight conditions.

# Deliverables

### Sentinel CDC light trap

-For each treatment arm, randomly sample 5 clusters (ie, 15 total clusters).  
- From each randomly sampled cluster, sample 4 households (ie, 60 total households), plus 2 "backup" households, clearly indicated as such (ie, 90 total households).  
- Ensure no overlap with those households sampled from the "Resting household indoor" selection (below), and also no overlap with those households sampled from the "Resting household pit shelter" households (far below).        
- **Deliverable 1**: A table in which each row is one household, with columns indicated `household ID`, `cluster`, `treatment arm`, `randomization number`, `randomization activity = "Sentinel CDC light trap"`, `backup`.
- **Deliverable 2**: An operational list, ie a copy of the above table but without `arm` since this needs to be blinded. This table may need to be re-generated periodically as households can be dropped out and replaced by backups.  


### Resting household indoor

-For each treatment arm, randomly sample 5 clusters (ie, 15 total clusters).  
- From each randomly sampled cluster, sample 4 households (ie, 60 total households), plus 2 backup households clearly indicated as such (ie, 90 total households).   
- Ensure no overlap with those households sampled from the "Sentinel CDC light trap" selection (above)   
- **Deliverable**: A table in which each row is one household, with columns indicated `household ID`, `cluster`, `treatment arm`, `randomization number`, `randomization activity = "Resting household indoor"`, `backup`.  
- **Deliverable 2**: An operational list, ie a copy of the above table but without `arm` since this needs to be blinded.  This table may need to be re-generated periodically as households can be dropped out and replaced by backups.  


### Resting household pit shelter

-For each treatment arm, randomly sample 5 clusters (ie, 15 total clusters).  
- From each randomly sampled cluster, sample 1 household (ie, 15 total households).   
- Ensure no overlap with those households sampled from the "Sentinel CDC light trap" selection (far above)   
- **Deliverable**: A table in which each row is one household, with columns indicated `household ID`, `cluster`, `treatment arm`, `randomization number`, `randomization activity = "Resting household pit shelter"`.  
- **Deliverable 2**: An operational list, ie a copy of the above table but without `arm` since this needs to be blinded.  This table may need to be re-generated periodically as households can drop out and replaced by neighboring households.  


### CDC Light Trap Livestock Enclosures

-For each treatment arm, randomly sample 2 clusters (ie, 6 total clusters).  
- From each randomly sampled cluster, sample 2 livestock enclosures (ie, 12 total livestock enclosures).   
- **Deliverable**: A table in which each row is one livestock enclosure, with columns indicated `livestock enclosure ID`, `cluster`, `treatment arm`, `randomization number`, `randomization activity = "CDC Light Trap Livestock Enclosures"`, `nearest household ID`.  
- **Deliverable 2**: An operational list, ie a copy of the above table but without `arm` since this needs to be blinded.  This table may need to be re-generated periodically as livestock enclosures can be dropped out and replaced by neighboring enclosures.  



### Human baited double net

-For each treatment arm, randomly sample 2 clusters (ie, 6 total clusters).  
- **Deliverable**: A table in which each row is one cluster enclosure, with columns indicating `cluster`, `treatment arm`, `randomization number`, `randomization activity = "Human baited double net"`.  
- **Deliverable 2**: An operational list with all households in the selected cluster  
- **Deliverable 3**: An operational list populated with data from forms B1 and B2 (**Clarification required**: what does this list contain)  



### Random CDC Light trap households

(not to be confused with _sentinentel_ CDC light trap households)

-For each treatment arm, randomly sample 3 clusters (ie, 9 total clusters), but do _not_ sample any of the sentinel clusters (**clarification required: avoid the sentinenel _clusters_ entirely, or just exclude sampled households from the sentinel clusters**).   
- From each sampled cluster, randomly sample 2 households (ie, 18 total households) plus 2 backups (ie, 36 total households) clearly indicated as such.  
- Repeat the above 15 times, so that there is a randomization for each of the 15 study months.  
- **Deliverable**: A table in which each row is one household-month combination, with columns indicating `month_number`, `household ID`,  `cluster`, `treatment arm`, `randomization number`, `randomization activity = "Random CDC Light trap households"`, `backup`.


### Larval habitats (water bodies)

-For each treatment arm, randomly sample at least 20 larval habitats  
- (**clarification required: how many to sample, exactly**)  
- **Deliverable**: (**Clarification required**: incomplete sentences in "Ento randomization, sampling, and list generation.pptx" pertaining to what the list should consist of, what "showing groups mean")  
